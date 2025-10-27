// forward decl
void test(int param1, int param2);
// pass by pointer
// void test2(int *arr, int size);

void main () {
  int x = 5;
  int b, c = 3;
  float a;
  a = x;
  test(a, b);

  // If statement tests

  if(x > 0) {
    b = b + 1;
  } else {
    c = c - 1;
  }

  if (x == 0) {
    a = a + 1.0;
  }

  // Do-while and while loop tests

  do {
    x = x - 1;
  } while(x > 0);

  while(x < 10) {
    x = x + 1;
  }

  // For loop tests

  for(int i = 0; i < 10; i = i + 1) {
    a = a + 2.0;
  }
  for (; x < 20; ) {
    x = x + 2;
  }
  for (int j = 0; j < 5; ) {
    b = b + 2;
  }

  // Array test

  int arrr[];
  int arr[5];
  arr[0] = 10;

  // Casting tests

  a = (float)x;
}

void test(int param1, int param2) {
  return param1 + param2;
}

// void test2(int *arr, int size) {
//   for(int i = 0; i < size; i = i + 1) {
//     arr[i] = i;
//   }
// }