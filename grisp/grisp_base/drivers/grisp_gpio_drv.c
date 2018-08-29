/* grisp_gpio_drv.c */

/*=== INCLUDES ==============================================================*/

#include <assert.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/ioctl.h>

#include <rtems.h>
#include <bsp.h>
#include <bsp/pin-config.h>

#include "erl_driver.h"
#include "sys.h"

/*=== MACROS ================================================================*/

#define RESP_MSG_CODE     'R'
#define EVENT_MSG_CODE    'E'

#define CMD_START_POLLING   1
#define CMD_STOP_POLLING    2
#define CMD_PIN_CONFIG     10
#define CMD_PIN_GET        11
#define CMD_PIN_CLEAR      12
#define CMD_PIN_SET        13

#define TIMER_NAME rtems_build_name('G', 'G', 'T', 'I') // Grisp Gpio TImer
#define QUEUE_NAME rtems_build_name('G','G','Q','U')    // Grisp Gpio QUeue
#define TASK_NAME rtems_build_name('G','G','T','A')     // Grisp Gpio TAsk
#define TASK_PRIO 8
#define TASK_STACK 1024
#define MSG_SIZE 3

#define DEFAULT_POLLING_INTERVAL 500 // ms

/*=== FORWARD DECLARATIONS ==================================================*/

/* Structures */
struct grisp_gpio_message;
struct grisp_gpio_pin_state;
struct grisp_gpio_data;

/* Erlang Driver Functions */
int grisp_gpio_init(void);
ErlDrvData grisp_gpio_start(ErlDrvPort port, char *command);
void grisp_gpio_stop(ErlDrvData drv_data);
void grisp_gpio_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
void grisp_gpio_ready_input(ErlDrvData data, ErlDrvEvent event);
void grisp_gpio_stop_select(ErlDrvEvent event, void* reserved);

/* Internal Functions */
static char* gpio_configure(struct grisp_gpio_data *data,
                            uint8_t index, uint8_t type, uint8_t attribute,
                            uint8_t high_trigger, uint8_t low_trigger,
                            uint32_t poll_interval,
                            uint32_t debounce_iterations);
static uint8_t gpio_get(struct grisp_gpio_data *data, uint8_t index);
static void gpio_clear(struct grisp_gpio_data *data, uint8_t index);
static void gpio_set(struct grisp_gpio_data *data, uint8_t index);
static char* start_polling(struct grisp_gpio_data *data);
static void stop_polling(struct grisp_gpio_data *data);
static bool is_polling(struct grisp_gpio_data *data);
static char* schedule_timer(struct grisp_gpio_data *data);
static void update_polling_interval(struct grisp_gpio_data *data);
static uint32_t gcd(uint32_t a, uint32_t b);
static uint32_t read_little_endian_uint32(uint8_t **pp);

/* RTEMS Functions */
rtems_timer_service_routine grisp_gpio_trigger(rtems_id id, void* arg);
rtems_task grisp_gpio_task(rtems_task_argument arg);

/*=== PINS DEFINITION =======================================================*/

static Pin pins[] =
  {
    {PIO_PC12,             PIOC, ID_PIOC, PIO_INPUT,    PIO_DEFAULT}, /* 0: gpio1 pin1 */
    {PIO_PC13,             PIOC, ID_PIOC, PIO_INPUT,    PIO_DEFAULT}, /* 1: gpio1 pin2 */
    {PIO_PA21,             PIOA, ID_PIOA, PIO_INPUT,    PIO_DEFAULT}, /* 2: gpio1 pin3 */
    {PIO_PD30,             PIOD, ID_PIOD, PIO_INPUT,    PIO_DEFAULT}, /* 3: gpio1 pin4 */
    {PIO_PD0,              PIOD, ID_PIOD, PIO_INPUT,    PIO_DEFAULT}, /* 4: gpio2 pin1 */
    {PIO_PD1,              PIOD, ID_PIOD, PIO_INPUT,    PIO_DEFAULT}, /* 5: gpio2 pin2 */
    {PIO_PD2,              PIOD, ID_PIOD, PIO_INPUT,    PIO_DEFAULT}, /* 6: gpio2 pin3 */
    {PIO_PD3,              PIOD, ID_PIOD, PIO_INPUT,    PIO_DEFAULT}, /* 7: gpio2 pin4 */
    {PIO_PA24,             PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 8:  led1 r */
    {PIO_PA17,             PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 9:  led1 g */
    {PIO_PA23,             PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 10: led1 b */
    {PIO_PA13,             PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 11: led2 r */
    {PIO_PA5 ,             PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 12  led2 g */
    {PIO_PA11,             PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 13: led2 b */
    {PIO_PA6 ,             PIOA, ID_PIOA, PIO_INPUT,    PIO_PULLUP},  /* 14: jumper 1 */
    {PIO_PD9 ,             PIOD, ID_PIOD, PIO_INPUT,    PIO_PULLUP},  /* 15: jumper 2 */
    {PIO_PC17,             PIOC, ID_PIOC, PIO_INPUT,    PIO_PULLUP},  /* 16: jumper 3 */
    {PIO_PD11,             PIOD, ID_PIOD, PIO_INPUT,    PIO_PULLUP},  /* 17: jumper 4 */
    {PIO_PC10,             PIOC, ID_PIOC, PIO_INPUT,    PIO_PULLUP},  /* 18: jumper 5 */
    {PIO_PA9,              PIOA, ID_PIOA, PIO_INPUT,    PIO_DEFAULT}, /* 19: spi1 pin 9 */
    {PIO_PA10,             PIOA, ID_PIOA, PIO_INPUT,    PIO_DEFAULT}, /* 20: spi1 pin10 */
    {PIO_PD12C_SPI0_NPCS2, PIOD, ID_PIOD, PIO_PERIPH_C, PIO_DEFAULT}, /* 21: SS1 */
    {PIO_PD27B_SPI0_NPCS3, PIOD, ID_PIOD, PIO_PERIPH_B, PIO_DEFAULT}, /* 22: SS2 */
    {PIO_PB2C_CTS0,        PIOB, ID_PIOB, PIO_PERIPH_C, PIO_DEFAULT}, /* 23: uart_1_cts */
    {PIO_PB1C_TXD0,        PIOB, ID_PIOB, PIO_PERIPH_C, PIO_DEFAULT}, /* 24: uart_2_txd */
    {PIO_PB0C_RXD0,        PIOB, ID_PIOB, PIO_PERIPH_C, PIO_DEFAULT}, /* 25: uart_3_rxd */
    {PIO_PB3C_RTS0,        PIOB, ID_PIOB, PIO_PERIPH_C, PIO_DEFAULT}  /* 26: uart_4_rts */
  };

#define N_PINS (sizeof(pins)/sizeof(pins[0]))

/*=== TYPES =================================================================*/

struct grisp_gpio_message {
    uint8_t index;
    uint8_t value;
};

struct grisp_gpio_pin_state {
    uint8_t polling_enabled;
    uint8_t high_trigger;
    uint8_t low_trigger;
    uint8_t last_value;
    uint32_t poll_interval;
    uint32_t debounce_iterations;
    uint32_t debounce_counter;
    uint32_t debounce_target;
};

struct grisp_gpio_data {
    ErlDrvPort port;
    int fd[2];
    uint32_t poll_interval; // ms
    uint32_t poll_ticks;
    Pin* pins_config;
    struct grisp_gpio_pin_state pins_state[N_PINS];
    struct grisp_rtems_event_arg *event;
    rtems_id timer_id;
    rtems_id task_id;
    rtems_id queue_id;
    size_t used_buff;
    char buff[MSG_SIZE];
};

/*=== GLOBAL VARIABLES ======================================================*/

ErlDrvEntry grisp_gpio_driver_entry = {
    grisp_gpio_init,                    /* init callback */
    grisp_gpio_start,                   /* start callback */
    grisp_gpio_stop,                    /* stop callback */
    grisp_gpio_output,                  /* output callback */
    grisp_gpio_ready_input,             /* ready_input callback */
    NULL,                               /* ready_output callback */
    "grisp_gpio_drv",                   /* driver_name */
    NULL,                               /* finish callback */
    NULL,                               /* reserved */
    NULL,                               /* control callback */
    NULL,                               /* timeout callback */
    NULL,                               /* outputv callback */
    NULL,                               /* ready_async callback */
    NULL,                               /* flush callback */
    NULL,                               /* call callback */
    NULL,                               /* unused */
    ERL_DRV_EXTENDED_MARKER,            /* extended_marker */
    ERL_DRV_EXTENDED_MAJOR_VERSION,     /* major_version */
    ERL_DRV_EXTENDED_MINOR_VERSION,     /* minor_version */
    0,                                  /* driver_flags */
    NULL,                               /* reserved */
    NULL,                               /* process_exit callback */
    grisp_gpio_stop_select,             /* stop_select callback */
    NULL                                /* emergency_close callback */
};

static struct grisp_gpio_data grisp_gpio_data = { NULL };

/*=== ERLANG DRIVER FUNCTIONS ===============================================*/

int
grisp_gpio_init(void)
{
    /* Asserts RTEMS timers have at least milisecond resolution */
    ASSERT (RTEMS_MILLISECONDS_TO_TICKS(1) * 2 == RTEMS_MILLISECONDS_TO_TICKS(2));
    return 0;
}

ErlDrvData
grisp_gpio_start(ErlDrvPort port, char *command)
{
    struct grisp_gpio_data *data = &grisp_gpio_data;
    int i;

    if (data->port != NULL)
        return ERL_DRV_ERROR_GENERAL;

    data->port = port;
    data->pins_config = pins;

    update_polling_interval(data);

    return (ErlDrvData)data;
}

void
grisp_gpio_stop(ErlDrvData drv_data)
{
    struct grisp_gpio_data *data = &grisp_gpio_data;
    ASSERT ((struct grisp_gpio_data *) drv_data == data);

    stop_polling(data);

    data->port = NULL;
}

void
grisp_gpio_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    uint8_t *p = (uint8_t *) buf;
    uint8_t cmd, index, type, attribute, high_trigger, low_trigger;
    uint32_t poll_interval, debounce_iterations;
    char *error;
    uint8_t res[2] = {(uint8_t) RESP_MSG_CODE, 0};

    struct grisp_gpio_data *data = &grisp_gpio_data;
    ASSERT ((struct grisp_gpio_data *) drv_data == data);
    ASSERT (data->port != NULL);

    ASSERT (len >= 1);
    cmd = *p++;

    if (cmd >= CMD_PIN_CONFIG) {
        ASSERT (len >= 2);
        index = *p++;
        if (index >= N_PINS) {
            driver_failure_atom(data->port, "pin_index_range");
            return;
        }
    }

    switch (cmd) {
    case CMD_START_POLLING:
        error = start_polling(data);
        if (error != NULL) {
            driver_failure_atom(data->port, error);
            return;
        }
        driver_output(data->port, (char *) res, 1);
        break;
    case CMD_STOP_POLLING:
        stop_polling(data);
        driver_output(data->port, (char *) res, 1);
        break;
    case CMD_PIN_CONFIG:
        ASSERT (len >= 14);
        type = *p++;
        attribute = *p++;
        high_trigger = *p++;
        low_trigger = *p++;
        poll_interval = read_little_endian_uint32(&p);
        debounce_iterations = read_little_endian_uint32(&p);
        error = gpio_configure(data, index, type, attribute,
                               high_trigger, low_trigger,
                               poll_interval, debounce_iterations);
        if (error != NULL) {
            driver_failure_atom(data->port, error);
            return;
        }
        driver_output(data->port, (char *) res, 1);
        break;
    case CMD_PIN_GET:
        res[2] = gpio_get(data, index);
        driver_output(data->port, (char *) res, 2);
        break;
    case CMD_PIN_CLEAR:
        gpio_clear(data, index);
        driver_output(data->port, (char *) res, 1);
        break;
    case CMD_PIN_SET:
        gpio_set(data, index);
        driver_output(data->port, (char *) res, 1);
        break;
    default:
        driver_failure_atom(data->port, "unknown_command");
        return;
    }
}

void
grisp_gpio_ready_input(ErlDrvData arg, ErlDrvEvent event)
{
    struct grisp_gpio_data *data = (struct grisp_gpio_data *) arg;
    ssize_t len, expected;

    while (true) {
        expected = MSG_SIZE - data->used_buff;
        len = read(data->fd[0], data->buff + data->used_buff, expected);

        if (len < 0) {
            if (errno == EWOULDBLOCK || errno == EINTR) break;
            driver_failure_posix(data->port, errno);
            return;
        }

        if ((data->used_buff + len) == MSG_SIZE) {
            data->used_buff = 0;
            driver_output(data->port, data->buff, MSG_SIZE);
            continue;
        }

        data->used_buff += len;
        break;
    }
}

void
grisp_gpio_stop_select (ErlDrvEvent event, void* reserved)
{
    close((int)event);
}

/*=== INTERNAL FUNCTIONS ====================================================*/

static char*
gpio_configure(struct grisp_gpio_data *data,
               uint8_t index, uint8_t type, uint8_t attribute,
               uint8_t high_trigger, uint8_t low_trigger,
               uint32_t poll_interval, uint32_t debounce_iterations)
{
    rtems_interrupt_level level;
    struct grisp_gpio_pin_state *pin_state;
    Pin *pin_config;
    uint8_t was_enabled;

    ASSERT ((high_trigger == 1) || (high_trigger == 0));
    ASSERT ((low_trigger == 1) || (low_trigger == 0));
    ASSERT (poll_interval > 0);

    was_enabled = pin_state->polling_enabled;
    pin_config = &data->pins_config[index];
    pin_state = &data->pins_state[index];

    rtems_interrupt_disable(level);

    pin_config->type = type;
    pin_config->attribute = attribute;
    PIO_Configure(data->pins_config + index, 1);

    if (high_trigger || low_trigger) {
        pin_state->polling_enabled = 1;
        pin_state->high_trigger = high_trigger;
        pin_state->low_trigger = low_trigger;
        pin_state->last_value = 0;
        pin_state->poll_interval = poll_interval;
        pin_state->debounce_iterations = debounce_iterations;
        pin_state->debounce_counter = 0;

        if ((poll_interval % data->poll_interval) == 0) {
            pin_state->debounce_target =
                debounce_iterations * (poll_interval / data->poll_interval);
        } else {
            update_polling_interval(data);
        }
    } else {
        pin_state->polling_enabled = 0;
        if (was_enabled) {
            update_polling_interval(data);
        }
    }

    rtems_interrupt_enable(level);

    return NULL;
}

static uint8_t
gpio_get(struct grisp_gpio_data *data, uint8_t index)
{
    return PIO_Get(data->pins_config + index);
}

static void
gpio_clear(struct grisp_gpio_data *data, uint8_t index)
{
    PIO_Clear(data->pins_config + index);
}

static void
gpio_set(struct grisp_gpio_data *data, uint8_t index)
{
    PIO_Set(data->pins_config + index);
}

static char*
start_polling(struct grisp_gpio_data *data)
{
    rtems_status_code status;
    int pipe_flags;

    if (is_polling(data)) return NULL;

    ASSERT (data->timer_id == NULL);
    ASSERT (data->task_id == NULL);
    ASSERT (data->queue_id == NULL);

    if (pipe(data->fd))
        return "pipe_allocation_failure";

    pipe_flags = fcntl(data->fd[0], F_GETFL, 0);
    if (fcntl(data->fd[0], F_SETFL, pipe_flags | O_NONBLOCK) == -1)
        return "pipe_configuration_failure";

    driver_select(data->port, (ErlDrvEvent)data->fd[0], ERL_DRV_READ, 1);

    status = rtems_message_queue_create(QUEUE_NAME, 16,
                                        sizeof(struct grisp_gpio_message),
                                        RTEMS_PRIORITY, &data->queue_id);
    if (status != RTEMS_SUCCESSFUL) return "rtems_queue_create_failure";

    status = rtems_timer_create(TIMER_NAME, &data->timer_id);
    if (status != RTEMS_SUCCESSFUL) return "rtems_timer_create_failure";

    status = rtems_task_create(TASK_NAME, TASK_PRIO, TASK_STACK,
                               RTEMS_DEFAULT_MODES, RTEMS_DEFAULT_ATTRIBUTES,
                               &data->task_id);
    if (status != RTEMS_SUCCESSFUL) return "rtems_task_create_failure";

    status = rtems_task_start(data->task_id, &grisp_gpio_task,
                              (rtems_task_argument) data);
    if (status != RTEMS_SUCCESSFUL) return "rtems_task_start_failure";

    return schedule_timer(data);
}

static void
stop_polling(struct grisp_gpio_data *data)
{
    if (data->timer_id != (rtems_id) NULL) {
        rtems_timer_delete(data->timer_id);
        data->timer_id = (rtems_id) NULL;
    }

    if (data->task_id != (rtems_id) NULL) {
        rtems_task_delete(data->task_id);
        data->task_id = (rtems_id) NULL;
    }

    if (data->queue_id != (rtems_id) NULL) {
        rtems_message_queue_delete(data->queue_id);
        data->queue_id = (rtems_id) NULL;
    }

    if (data->fd[0] != (int) NULL) {
        ASSERT (data->fd[1] != (int) NULL);
        driver_select(data->port, (ErlDrvEvent)data->fd[0], ERL_DRV_READ, 0);
        /* fd[0] will get closed by the stop_select callback */
        close(data->fd[1]);
        data->fd[0] = (int) NULL;
        data->fd[1] = (int) NULL;
    }
}

static bool
is_polling(struct grisp_gpio_data *data)
{
    return data->timer_id != (int) NULL;
}

static char*
schedule_timer(struct grisp_gpio_data *data)
{
    rtems_status_code status;

    status = rtems_timer_fire_after(data->timer_id, data->poll_ticks,
                                    &grisp_gpio_trigger, (void *) data);
    if (status != RTEMS_SUCCESSFUL) return "rtems_timer_scheduling_failure";

    return NULL;
}

static void
update_polling_interval(struct grisp_gpio_data *data)
{
    struct grisp_gpio_pin_state* pin_state;
    uint32_t factor, interval = 0;
    uint8_t i;

    /* Find the common interval for all enabled pins */
    for (i = 0; i < N_PINS; i++) {
        pin_state = &data->pins_state[i];
        if (pin_state->polling_enabled) {
            if (interval == 0) {
                interval = pin_state->poll_interval;
            } else {
                interval = gcd(interval, pin_state->poll_interval);
            }
        }
    }

    if (interval == 0) {
        data->poll_interval = DEFAULT_POLLING_INTERVAL;
        data->poll_ticks =
            RTEMS_MILLISECONDS_TO_TICKS(DEFAULT_POLLING_INTERVAL);
        return;
    }

    data->poll_interval = interval;
    data->poll_ticks = RTEMS_MILLISECONDS_TO_TICKS(interval);

    for (i = 0; i < N_PINS; i++) {
        pin_state = &data->pins_state[i];
        if (pin_state->polling_enabled) {
            factor = pin_state->poll_interval / interval;
            pin_state->debounce_target = pin_state->debounce_iterations * factor;
        }
    }
}

static uint32_t
gcd(uint32_t a, uint32_t b)
{
    uint32_t temp;
    while (b != 0) {
        temp = a % b;
        a = b;
        b = temp;
    }
    return a;
}

static uint32_t
read_little_endian_uint32(uint8_t **pp)
{
    uint8_t *buff = *pp;
    *pp += 4;
    return buff[0] + buff[1] * 256 + buff[2] * 65536 + buff[3] * 16777216;
}

/*=== RTEMS FUNCTIONS =======================================================*/

rtems_timer_service_routine
grisp_gpio_trigger(rtems_id id, void* arg)
{
    struct grisp_gpio_data* data = (struct grisp_gpio_data *) arg;
    struct grisp_gpio_message msg;
    struct grisp_gpio_pin_state* pin_state;
    Pin* pin_config;
    uint8_t pin_value;
    rtems_status_code status;
    char* error;
    uint8_t i;

    /* to not get a warning for variables only used in asserts */
    (void) error;
    (void) status;

    for (i = 0; i < N_PINS; i++) {
        pin_config = &data->pins_config[i];
        pin_state = &data->pins_state[i];

        if (!pin_state->polling_enabled) {
            continue;
        }

        pin_value = PIO_Get(pin_config);

        if (pin_state->last_value == pin_value) {
            pin_state->debounce_counter = 0;
            continue;
        }

        pin_state->debounce_counter += 1;

        if (pin_state->debounce_counter >= pin_state->debounce_target) {
            pin_state->last_value = pin_value;
            if ((pin_state->high_trigger && pin_value)
                || (pin_state->low_trigger && !pin_value)) {
                msg.index = i;
                msg.value = pin_value;
                status = rtems_message_queue_send(data->queue_id, (char *) &msg,
                                                  sizeof(msg));
                ASSERT (status == RTEMS_SUCCESSFUL);
                continue;
            }
        }
    }
    error = schedule_timer(data);
    ASSERT (error == NULL);
}

rtems_task
grisp_gpio_task(rtems_task_argument arg)
{
	struct grisp_gpio_data* data = (struct grisp_gpio_data *) arg;
    struct grisp_gpio_message msg;
    rtems_status_code status;
    size_t size;
    char buff[MSG_SIZE];

    /* to not get a warning for variables only used in asserts */
    (void) status;

    while (true) {
        status = rtems_message_queue_receive(data->queue_id, &msg, &size,
                                             RTEMS_WAIT, RTEMS_NO_TIMEOUT);
        ASSERT (status == RTEMS_SUCCESSFUL);
        ASSERT (size == sizeof(msg));
        buff[0] = (char) EVENT_MSG_CODE;
        buff[1] = (char) msg.index;
        buff[2] = (char) msg.value;
        write(data->fd[1], buff, MSG_SIZE);
    }
}
