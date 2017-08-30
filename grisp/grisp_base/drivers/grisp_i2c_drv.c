/* i2c_drv.c */

#include <assert.h>
#include <fcntl.h>
#include <rtems.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/ioctl.h>

#include <bsp.h>
#include <bsp/atsam-i2c.h>
#include <bsp/i2c.h>
#include <dev/i2c/i2c.h>

#include "erl_driver.h"
#include "sys.h"

/* Make sure to keep this at sync with the -define(res_max_size.. in i2c.erl */
#define RES_MAX_SIZE 256

/* Make sure to keep this at sync with the -define(msgs_max_count.. in i2c.erl */
#define MSGS_MAX_COUNT 32

int grisp_i2c_init (void);
ErlDrvData grisp_i2c_start (ErlDrvPort port, char *command);
void grisp_i2c_stop (ErlDrvData drv_data);
void grisp_i2c_output (ErlDrvData drv_data, char *buf, ErlDrvSizeT len);

ErlDrvEntry grisp_i2c_driver_entry = {
    grisp_i2c_init,
    grisp_i2c_start,
    grisp_i2c_stop,
    grisp_i2c_output,
    NULL,
    NULL,
    "grisp_i2c_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL,
    NULL
};

struct grisp_i2c_data {
  ErlDrvPort port;
  char cnt;
  int fd;
};

/* FIXME: consider making this more dynamic, especially if generalized */
static struct grisp_i2c_data grisp_i2c_data = { NULL, 0, -1 };

int grisp_i2c_init (void)
{
  int rv;

  rv = atsam_register_i2c_0();
  assert(rv == 0);

  return 0;
}

ErlDrvData grisp_i2c_start (ErlDrvPort port, char *command)
{
    if (grisp_i2c_data.port != NULL)
        return ERL_DRV_ERROR_GENERAL;

    grisp_i2c_data.port = port;
    grisp_i2c_data.cnt = 1;

    grisp_i2c_data.fd = open(ATSAM_I2C_0_BUS_PATH, O_RDWR);
    assert(grisp_i2c_data.fd != -1);

    return (ErlDrvData)&grisp_i2c_data;
}

void grisp_i2c_stop (ErlDrvData drv_data)
{
    assert ((struct grisp_i2c_data *)drv_data == &grisp_i2c_data);
    close(grisp_i2c_data.fd);
    grisp_i2c_data.port = NULL;
}

static uint16_t ubig16(void *p)
{
  uint8_t *q = p;

  return (uint16_t)(q[0] << 8 | q[1]);
}

void grisp_i2c_output (ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    int rv;
    uint8_t *p, *q, *write_buf;
    struct i2c_msg msgs[MSGS_MAX_COUNT];
    uint8_t res[RES_MAX_SIZE];
    int data_len, buf_off, buf_len;
    struct i2c_rdwr_ioctl_data msgset;
    int i, msg_count;

    assert ((struct grisp_i2c_data *)drv_data == &grisp_i2c_data);
    assert (grisp_i2c_data.port != NULL);

    grisp_i2c_data.cnt++;

    data_len = ubig16(buf);
    write_buf = buf + 2;
    q = write_buf + data_len;
    msg_count = ubig16(q);
    q += 2;
    assert (msg_count <= MSGS_MAX_COUNT);
    assert (msg_count * 8 == len - (q - (uint8_t *)buf));
    p = res;
    for (i = 0; i < msg_count; i++)
      {
        msgs[i].addr = ubig16(q);
        q += 2;
        msgs[i].flags = ubig16(q);
        q += 2;
        buf_len = ubig16(q);
        q += 2;
        buf_off = ubig16(q);
        q += 2;
        msgs[i].len = (uint16_t)buf_len;
        if (msgs[i].flags & I2C_M_RD)
          {
            assert (p + buf_len < res + RES_MAX_SIZE);
            msgs[i].buf = p;
            p += buf_len;
          }

        else
          {
            assert(buf_off + buf_len <= data_len);
            msgs[i].buf = write_buf + buf_off;
          }
      }

    msgset.msgs = msgs;
    msgset.nmsgs = msg_count;
    rv = ioctl(grisp_i2c_data.fd, I2C_RDWR, &msgset);
    assert(rv == 0);

    driver_output(grisp_i2c_data.port, (char *)res, p - res);
}
