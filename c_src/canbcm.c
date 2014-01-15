#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <poll.h>
#include <err.h>
#include <math.h>

#include <net/if.h>

#include <erl_interface.h>
#include <ei.h>

#include <linux/can.h>
#include <linux/can/bcm.h>
#include <linux/can/error.h>

enum can_state
{
    CAN_CLOSED,
    CAN_OPEN
};

struct candev {
    enum can_state state;
    int fd;
    struct sockaddr_can caddr;
};

struct can_msg {
    struct bcm_msg_head msg_head;
    struct can_frame frame;
};

int can_open(struct candev *pin, const char *ifname);
int can_release(struct candev *pin);

/*
 * Erlang request/response processing
 */
#define BUF_SIZE 1024
struct erlcmd
{
    unsigned char buffer[BUF_SIZE];
    ssize_t index;
};

void erlcmd_send(ETERM *response);

// CAN functions

void can_init(struct candev *can)
{
    can->state = CAN_CLOSED;
    can->fd = -1;
}

int can_open(struct candev *can, const char *ifname)
{
    /* If not closed, then release whatever is currently open. */
    if (can->state != CAN_CLOSED)
	can_release(can);

    can->fd = socket(PF_CAN, SOCK_DGRAM, CAN_BCM);
    if (can->fd < 0)
	err(EXIT_FAILURE, "socket");

    struct ifreq ifr;
    strncpy(ifr.ifr_name, ifname, IFNAMSIZ);
    if (ioctl(can->fd, SIOCGIFINDEX, &ifr))
	err(EXIT_FAILURE, "ioctl(SIOCGIFINDEX) %s", ifname);

    struct sockaddr_can caddr;
    memset(&can->caddr, 0, sizeof(can->caddr));
    can->caddr.can_family = PF_CAN;
    can->caddr.can_ifindex = ifr.ifr_ifindex;

    if (connect(can->fd, (struct sockaddr *) &caddr, sizeof(caddr)) < 0)
	err(EXIT_FAILURE, "connect");

    can->state = CAN_OPEN;
    return 1;
}

int can_release(struct candev *can)
{
    if (can->state == CAN_CLOSED)
	return 1;

    if (can->fd != -1)
	close(can->fd);
    can->fd = -1;
    can->state = CAN_CLOSED;
    return 1;
}

/**
 * Called when something is received from the CAN device.
 *
 * @param can which can to check
 */
void can_process(struct candev *can)
{
    struct sockaddr_can caddr;
    socklen_t caddrlen = sizeof(caddr);
    struct can_msg msg;

    int len = recvfrom(can->fd, &msg, sizeof(msg), 0, (struct sockaddr *) &caddr, &caddrlen);

    // Not sure if this is expected or not.
    if (len < 0)
	err(EXIT_FAILURE, "recvfrom");

    // get timestamp??

    if (msg.msg_head.can_id & CAN_ERR_FLAG) {
	// Handle CAN errors.
	if (msg.frame.can_dlc == CAN_ERR_DLC)
	    fprintf(stderr, "Wrong DLC. What to do?\n");
	else {
	    ETERM *resp = erl_format("{can_error, ~i}", msg.msg_head.can_id);
	    erlcmd_send(resp);
	    erl_free_term(resp);
	}
    } else {
	// Good CAN message.
	int canid = (msg.msg_head.can_id & CAN_EFF_FLAG) ?
	    (msg.msg_head.can_id & CAN_EFF_MASK) : (msg.msg_head.can_id & CAN_SFF_MASK);

	ETERM *data = erl_mk_binary((const char *) msg.frame.data, msg.frame.can_dlc);
	ETERM *resp = erl_format("{can_frame, ~i, ~w}", canid, data);
	erlcmd_send(resp);
	erl_free_term(resp);
	erl_free_term(data);
    }
}

/**
 * Initialize an Erlang command handler.
 *
 * @param handler the structure to initialize
 */
void erlcmd_init(struct erlcmd *handler)
{
    erl_init(NULL, 0);
    memset(handler, 0, sizeof(*handler));
}

/**
 * @brief Synchronously send a response back to Erlang
 *
 * @param response what to send back
 */
void erlcmd_send(ETERM *response)
{
    unsigned char buf[1024];

    if (erl_encode(response, buf + sizeof(uint16_t)) == 0)
	errx(EXIT_FAILURE, "erl_encode");

    ssize_t len = erl_term_len(response);
    uint16_t be_len = htons(len);
    memcpy(buf, &be_len, sizeof(be_len));

    len += sizeof(uint16_t);
    ssize_t wrote = 0;
    do {
	ssize_t amount_written = write(STDOUT_FILENO, buf + wrote, len - wrote);
	if (amount_written < 0) {
	    if (errno == EINTR)
		continue;

	    err(EXIT_FAILURE, "write");
	}

	wrote += amount_written;
    } while (wrote < len);
}

static struct timeval seconds_to_tv(double seconds)
{
    struct timeval tv;

    double wholepart = floor(seconds);
    tv.tv_sec = (int) wholepart;
    tv.tv_usec = (int) ((seconds - wholepart) * 1000000.0);

    return tv;
}

/**
 * @brief Dispatch commands in the buffer
 * @return the number of bytes processed
 */
ssize_t erlcmd_dispatch(struct erlcmd *handler, struct candev *can)
{
    /* Check for length field */
    if (handler->index < sizeof(uint16_t))
	return 0;

    uint16_t be_len;
    memcpy(&be_len, handler->buffer, sizeof(uint16_t));
    ssize_t msglen = ntohs(be_len);
    if (msglen + sizeof(uint16_t) > sizeof(handler->buffer))
	errx(EXIT_FAILURE, "Message too long");

    /* Check whether we've received the entire message */
    if (msglen + sizeof(uint16_t) > handler->index)
	return 0;

    ETERM *emsg = erl_decode(handler->buffer + sizeof(uint16_t));
    if (emsg == NULL)
	errx(EXIT_FAILURE, "erl_decode");

    ETERM *emsgtype = erl_element(1, emsg);
    if (emsgtype == NULL)
	errx(EXIT_FAILURE, "erl_element(emsgtype)");

    if (strcmp(ERL_ATOM_PTR(emsgtype), "open") == 0) {
	// TODO: consider moving open to the command line arguments, since
	//       the port isn't too useful until open is called anyway.
	ETERM *eifname = erl_element(2, emsg);
	if (eifname == NULL)
	    errx(EXIT_FAILURE, "init: eifname was NULL");

	can_open(can, ERL_ATOM_PTR(eifname));

	erl_free_term(eifname);
    } else if (strcmp(ERL_ATOM_PTR(emsgtype), "release") == 0) {
	can_release(can);
    } else if (strcmp(ERL_ATOM_PTR(emsgtype), "send") == 0) {
	ETERM *ecanid = erl_element(2, emsg);
	ETERM *edata = erl_element(3, emsg);

	// Set CAN_EFF_FLAG if necessary?

	struct can_msg msg;
	memset(&msg, 0, sizeof(msg));
	msg.msg_head.opcode = TX_SEND;
	msg.msg_head.nframes = 1;
	msg.frame.can_id = msg.msg_head.can_id = ERL_INT_VALUE(ecanid);
	msg.frame.can_dlc = ERL_BIN_SIZE(edata);
	if (msg.frame.can_dlc > 8)
	    errx(EXIT_FAILURE, "Can't send more than 8 bytes: %d", msg.frame.can_dlc);
	memcpy(msg.frame.data, ERL_BIN_PTR(edata), msg.frame.can_dlc);

	if (sendto(can->fd,
		   &msg, sizeof(msg), 0,
		   (struct sockaddr *) &can->caddr, sizeof(can->caddr)) < 0)
	    err(EXIT_FAILURE, "sendto(TX_SEND)");

	erl_free_term(edata);
	erl_free_term(ecanid);
    } else if (strcmp(ERL_ATOM_PTR(emsgtype), "add_send_job") == 0) {
	ETERM *eperiod = erl_element(2, emsg);
	ETERM *ecanid = erl_element(3, emsg);
	ETERM *edata = erl_element(4, emsg);

	struct can_msg msg;
	memset(&msg, 0, sizeof(msg));
	msg.msg_head.opcode = TX_SETUP;
	msg.msg_head.flags = SETTIMER | STARTTIMER;
	msg.msg_head.ival2 = seconds_to_tv(ERL_FLOAT_VALUE(eperiod));
	msg.msg_head.nframes = 1;
	msg.frame.can_id = msg.msg_head.can_id = ERL_INT_VALUE(ecanid);
	msg.frame.can_dlc = ERL_BIN_SIZE(edata);
	if (msg.frame.can_dlc > 8)
	    errx(EXIT_FAILURE, "Can't send more than 8 bytes: %d", msg.frame.can_dlc);
	memcpy(msg.frame.data, ERL_BIN_PTR(edata), msg.frame.can_dlc);

	if (sendto(can->fd,
		   &msg, sizeof(msg), 0,
		   (struct sockaddr *) &can->caddr, sizeof(can->caddr)) < 0)
	    err(EXIT_FAILURE, "sendto(TX_SETUP)");

	erl_free_term(edata);
	erl_free_term(ecanid);
	erl_free_term(eperiod);
    } else if (strcmp(ERL_ATOM_PTR(emsgtype), "update_send_job") == 0) {
	ETERM *ecanid = erl_element(2, emsg);
	ETERM *edata = erl_element(3, emsg);

	struct can_msg msg;
	memset(&msg, 0, sizeof(msg));
	msg.msg_head.opcode = TX_SETUP;
	msg.msg_head.nframes = 1;
	msg.frame.can_id = msg.msg_head.can_id = ERL_INT_VALUE(ecanid);
	msg.frame.can_dlc = ERL_BIN_SIZE(edata);
	if (msg.frame.can_dlc > 8)
	    errx(EXIT_FAILURE, "Can't send more than 8 bytes: %d", msg.frame.can_dlc);
	memcpy(msg.frame.data, ERL_BIN_PTR(edata), msg.frame.can_dlc);

	if (sendto(can->fd,
		   &msg, sizeof(msg), 0,
		   (struct sockaddr *) &can->caddr, sizeof(can->caddr)) < 0)
	    err(EXIT_FAILURE, "sendto(TX_SETUP2)");

	erl_free_term(edata);
	erl_free_term(ecanid);
    } else if (strcmp(ERL_ATOM_PTR(emsgtype), "delete_send_job") == 0) {
	ETERM *ecanid = erl_element(2, emsg);

	struct can_msg msg;
	memset(&msg, 0, sizeof(msg));
	msg.msg_head.opcode = TX_DELETE;
	msg.frame.can_id = msg.msg_head.can_id = ERL_INT_VALUE(ecanid);

	if (sendto(can->fd,
		   &msg, sizeof(msg), 0,
		   (struct sockaddr *) &can->caddr, sizeof(can->caddr)) < 0)
	    err(EXIT_FAILURE, "sendto(TX_DELETE)");

	erl_free_term(ecanid);
    } else if (strcmp(ERL_ATOM_PTR(emsgtype), "subscribe") == 0) {
	ETERM *eperiod = erl_element(2, emsg);
	ETERM *ecanid = erl_element(3, emsg);

	// Set CAN_EFF_FLAG if necessary?

	struct can_msg msg;
	memset(&msg, 0, sizeof(msg));
	msg.msg_head.opcode = RX_SETUP;
	msg.msg_head.flags = RX_FILTER_ID | SETTIMER;
	msg.msg_head.nframes = 1;
	msg.msg_head.ival2 = seconds_to_tv(ERL_FLOAT_VALUE(eperiod));
	msg.frame.can_id = msg.msg_head.can_id = ERL_INT_VALUE(ecanid);
	if (sendto(can->fd,
		   &msg, sizeof(msg), 0,
		   (struct sockaddr *) &can->caddr, sizeof(can->caddr)) < 0)
	    err(EXIT_FAILURE, "sendto(RX_SETUP)");

	erl_free_term(eperiod);
	erl_free_term(ecanid);
    } else if (strcmp(ERL_ATOM_PTR(emsgtype), "unsubscribe") == 0) {
	ETERM *ecanid = erl_element(2, emsg);

	// Set CAN_EFF_FLAG if necessary?

	struct can_msg msg;
	memset(&msg, 0, sizeof(msg));
	msg.msg_head.opcode = RX_DELETE;
	msg.msg_head.nframes = 1;
	msg.frame.can_id = msg.msg_head.can_id = ERL_INT_VALUE(ecanid);
	if (sendto(can->fd,
		   &msg, sizeof(msg), 0,
		   (struct sockaddr *) &can->caddr, sizeof(can->caddr)) < 0)
	    err(EXIT_FAILURE, "sendto(RX_DELETE)");

	erl_free_term(ecanid);
    } else {
	errx(EXIT_FAILURE, "unexpected element: %s", ERL_ATOM_PTR(emsgtype));
    }

     erl_free_term(emsg);
     erl_free_term(emsgtype);

     return msglen + sizeof(uint16_t);
}

/**
 * @brief call to process any new requests from Erlang
 */
void erlcmd_process(struct erlcmd *handler, struct candev *can)
{
    ssize_t amount_read = read(STDIN_FILENO, handler->buffer, sizeof(handler->buffer) - handler->index);
    if (amount_read < 0) {
	/* EINTR is ok to get, since we were interrupted by a signal. */
	if (errno == EINTR)
	    return;

	/* Everything else is unexpected. */
	err(EXIT_FAILURE, "read");
    } else if (amount_read == 0) {
	/* EOF. Erlang process was terminated. This happens after a
	   release or if there was an error. */
	exit(EXIT_SUCCESS);
    }

    handler->index += amount_read;
    for (;;) {
	ssize_t bytes_processed = erlcmd_dispatch(handler, can);
	if (bytes_processed == 0) {
	    /* Only have part of the command to process. */
	    break;
	} else if (handler->index > bytes_processed) {
	    /* Processed the command and there's more data. */
	    memmove(handler->buffer, &handler->buffer[bytes_processed], handler->index - bytes_processed);
	    handler->index -= bytes_processed;
	} else {
	    /* Processed the whole buffer. */
	    handler->index = 0;
	    break;
	}
    }
}

/**
 * @brief The main function.
 * It waits for data in the buffer and calls the driver.
 */
int main()
{
    struct candev can;
    struct erlcmd handler;

    erlcmd_init(&handler);
    can_init(&can);

    for (;;) {
	struct pollfd fdset[2];

	fdset[0].fd = STDIN_FILENO;
	fdset[0].events = POLLIN;
	fdset[0].revents = 0;

	fdset[1].fd = can.fd;
	fdset[1].events = POLLIN;
	fdset[1].revents = 0;

	/* Always fill out the fdset structure, but only check the
	 * CAN file descriptor if opened.
	 */
	int rc = poll(fdset, can.state != CAN_CLOSED ? 2 : 1, -1);
	if (rc < 0) {
	    /* Retry if EINTR */
	    if (errno == EINTR)
		continue;

	    err(EXIT_FAILURE, "poll");
	}

	if (fdset[0].revents & (POLLIN | POLLHUP))
	    erlcmd_process(&handler, &can);

	if (fdset[1].revents & (POLLIN | POLLHUP))
	    can_process(&can);
    }

    return 0;
}
