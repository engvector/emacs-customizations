/* test.cpp - file to test lsp server mode */

#include <iostream>
#include <string>

using namespace std;

void printstring(string);

int main()
{
  cout << "Hello World" << endl;
  printstring("String passed to function");

  return 0;
}

void printstring(string message) { cout << message << endl; }
