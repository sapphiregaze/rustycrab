// pass by pointer
void test2(int *arr, int size);

void main () {
  // pointer test
  int x = 10;
  int* p;
  p = &x;
}

void test (int param1, int param2) {
  return param1 + param2;
}

void test2(int *x) {
  return *x + 1;
}