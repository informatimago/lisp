#define F(X) [X]
#define G(X) (X)
#define H(M,X) M(X)

H(F,42)
// --> [42]
H(G,33)
// --> (33)
