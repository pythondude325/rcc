// code: 1
// ignore: https://github.com/jyn514/rcc/issues/53
struct s {
        int i;
        float f;
        union { int a; float b; } u;
    } m = { 1, 2.4, { 3 } };
    int main() { return m.i; }
