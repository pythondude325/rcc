// fail
struct s {
    int f();
} my_s;

int main() {
    return my_s.f();
}
