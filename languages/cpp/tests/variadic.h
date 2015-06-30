#define POO0(format,arg) printf(format,arg)
#define POO1(format, ...) printf(format, ## __VA_ARGS__)
#define POO2(format, args...) printf(format, ## args)
#define POO3(format, args) printf(format, ## args) // only works for variadic parameter.
