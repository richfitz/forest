// Copyright 2007-2008 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License")
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an AS IS BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Author: madscience@google.com (Moshe Looks)

/****
     Simple utility reading from stdin and writing to stdout that interconverts
     between s-expression formats, e.g.:

     (foo bar (baz goo)) <--> foo(bar baz(goo))

     +(1 *(x /(y z))) <--> (+ 1 (* x (/ y z)))

****/

#include "tree_io.hpp"
#include <iostream>

using namespace std;
using namespace TREE_TREE_NAMESPACE;

void eat_whitespace() {
  char c;
  do {
    cin >> c;    
  } while (cin && _tree_io_private::whitespace(c));
  cin.putback(c);
}

int main(int argc,char** argv) {
  tree<string> tr;
  eat_whitespace();
  while (cin) {
    if (cin.peek()=='(') {
      cin >> sexpr_format(tr);
      cout << tr << endl;
    } else {
      cin >> tr;
      cout << sexpr_format(tr) << endl;
    }
    eat_whitespace();
  }
}
