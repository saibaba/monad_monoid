/*

In this example:  safeHead is a transformation but not natural.

*/

// clang++ -std=c++17 <file>.cpp

// https://math.stackexchange.com/questions/2772832/non-natural-transformation-example

#include <vector>
#include <iostream>
#include <algorithm> 
#include <optional>
#include <functional>


using namespace std;

template <class T>
optional<T> safeHead(vector<T> a)
{
  if (a.size() == 0)
  {
    return  nullopt;
  }

  return optional{a[0]};
}

template <>
optional<int> safeHead(vector<int> a)
{
  return optional{5};
}


int inc(int v)
{
  return v+1;
}

string show(int v)
{
  return to_string(v);
}

void printer(int i) 
{
  cout << " " << i;
}


template <class T>
void print_optional(optional<T> iv)
{
  cout << iv.value() << endl;
}

template<typename T, typename F>
auto transform(const std::optional<T>& in, F h)
{
    return in.has_value() ? std::optional{h(*in)} : std::nullopt;
}


void test_inc()
{
  vector<int> input_ints{1, 2, 3};

  optional<int> tmp = safeHead(input_ints);
  optional<int> path1_output = transform(tmp, inc);
  print_optional(path1_output);

  //////////////

  vector<int> itmp;
  itmp.resize(input_ints.size());
  std::transform(input_ints.begin(), input_ints.end(), itmp.begin(), inc);
  optional<int> path2_output = safeHead(itmp);
  print_optional(path2_output);
}

void test_show()
{
  vector<int> input_ints{1, 2, 3};

  optional<int> tmp = safeHead(input_ints);
  optional<string> path1_output = transform(tmp, show);
  print_optional(path1_output);

  //////////////

  vector<string> itmp;
  itmp.resize(input_ints.size());
  std::transform(input_ints.begin(), input_ints.end(), itmp.begin(), show);
  optional<string> path2_output = safeHead(itmp);
  print_optional(path2_output);
}

int main(int argc, char **argv)
{

  test_inc();
  cout << endl << "----------" << endl;
  test_show();
  return 0;
}

