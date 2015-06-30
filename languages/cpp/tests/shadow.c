
#define FOO 42
#define BAR FOO
#define BABAR G(FOO)
#define G(BAZ) (BAZ,FOO,BAR)
#define F(FOO) ((FOO)+G(FOO)+BAR,BABAR)

F(33),FOO;
// --> ((33)+(42)),42;
