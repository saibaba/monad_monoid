/*

In this example:  ident is a transformation from a category to the same (vector here).
It works as expected for all types except for Integer (due to adhoc polymorphism).

So, it is a transformation but not natural as natural means it must work for all types.

*/

// clang++ -std=c++11 <file>.cpp

// https://math.stackexchange.com/questions/2772832/non-natural-transformation-example

#include <vector>
#include <iostream>
#include <algorithm> 

using namespace std;

template <class T>
vector<T> ident(vector<T> a)
{
  return a;
}


template <>
vector<int> ident(vector<int> a)
{
  return vector<int>{5};
}


int inc(int v)
{
  return v+1;
}

void printer(int i) 
{
  cout << " " << i;
}


void print_vec(vector<int> iv)
{
  std::for_each(iv.begin(), iv.end(), printer);
  cout << endl;
}

int main(int argc, char **argv)
{

  vector<int> input_ints{1, 2, 3};

  vector<int> path1_output;
  vector<int> tmp = ident(input_ints);
  path1_output.resize(tmp.size());
  std::transform(tmp.begin(), tmp.end(), path1_output.begin(), inc);
  print_vec(path1_output);

  tmp.clear();
  tmp.resize(input_ints.size());

  //////////////

  std::transform(input_ints.begin(), input_ints.end(), tmp.begin(), inc);
  vector<int> path2_output = ident(tmp);
  print_vec(path2_output);

  return 0;
}

