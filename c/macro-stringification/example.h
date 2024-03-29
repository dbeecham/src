#pragma once

#define XSTR(x) #x
#define STR(x) XSTR(x)

#ifndef CONFIG_NUM_TIMERS
#define CONFIG_NUM_TIMERS 1
#endif

#define CONFIG_NUM_TIMERS_STRING STR(CONFIG_NUM_TIMERS)
#define CONFIG_NUM_TIMERS_FMT ("(1<<32) * " CONFIG_NUM_TIMERS_STRING)
