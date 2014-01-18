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
     First comes a generic testing prologue, then a bunch of test cases
     exercising tree capabilities.
****/

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/included/unit_test.hpp>
#include <boost/assign/std/vector.hpp>
#include "foreach.hpp"
#include "tree_io.hpp"
#include "tree_iterator.hpp"

#define test_case BOOST_AUTO_TEST_CASE
#define check BOOST_CHECK
#define check_eq BOOST_CHECK_EQUAL
#define check_throw(x) BOOST_CHECK_THROW(x,std::runtime_error)
#define check_tree(tr,sz,name)              \
{                                           \
  check_eq(tr.empty(),(sz==0));             \
  check_eq(tr.size(),(unsigned int)sz);     \
  check_eq(tr.childless(),(sz<=1));         \
  check_eq(lexical_cast<string>(tr),name);  \
}

using namespace std;
using namespace boost;
using namespace boost::assign;
using namespace TREE_TREE_NAMESPACE;

typedef tree<int> itree;
typedef subtree<int> isubtree;

// The tests - note that tree structure
//      1      
//     / \      
//    2   5    
//   / \       
//  3   4      
//             
// is written as 1(2(3 4) 5).

test_case(tree_empty_ctor) {
  itree tr;
  check_tree(tr,0,"");

  itree::const_pre_iterator it=tr.begin();

  const itree& tr1(tr);

  itree::const_pre_iterator it1=tr1.begin();
}

test_case(tree_root_ctor) {
  itree tr(42);
  check_tree(tr,1,"42");
  check_eq(tr.root(),42);
}

test_case(tree_insert1) {
  itree tr;

  tr.insert(tr.end(),1);
  check_tree(tr,1,"1");

  tr.insert(tr.end_child(),2);
  check_tree(tr,2,"1(2)");

  tr.insert(tr.end_child(),3);
  check_tree(tr,3,"1(2 3)");

  tr.insert(tr.begin_sub_child()->end_child(),4);
  check_tree(tr,4,"1(2(4) 3)");
             
  tr.insert(tr.end_child(),5);
  check_tree(tr,5,"1(2(4) 3 5)");

  tr.insert(tr.begin_sub_child()->begin_child(),6);
  check_tree(tr,6,"1(2(6 4) 3 5)");
}

test_case(tree_copy_ctor) {
  itree tr;
  
  itree tr1(tr);
  check_eq(tr,tr1);

  tr.insert(tr.end(),1);
  itree tr2(tr);
  check_eq(tr,tr2);

  tr.insert(tr.end_child(),2);
  tr.insert(tr.end_child(),3);
  itree tr3(tr);
  check_eq(tr,tr3);
}

test_case(tree_assignment) {
  itree tr;
  
  itree tr1;
  tr1=tr;
  check_eq(tr,tr1);

  tr.insert(tr.end(),1);
  itree tr2(42);
  tr2=tr;
  check_eq(tr,tr2);

  tr.insert(tr.end_child(),2);
  tr.insert(tr.end_child(),3);
  itree tr3=tree_of(42)(4,5);
  tr3=tr;
  check_eq(tr,tr3);
}

test_case(tree_subtr_assignment) {
  itree tr=tree_of(5)(6,tree_of(7)(8));
  itree tr2=tree_of(1)(2,3);
  tr[0]=tr[1];
  check_tree(tr,5,"5(7(8) 7(8))");
  tr[1]=tr2;
  check_tree(tr,6,"5(7(8) 1(2 3))");  
}

test_case(tree_arity) {
  itree tr=tree_of(1)(2,3,tree_of(4)(5));
  check_tree(tr,5,"1(2 3 4(5))");

  check_eq(tr.arity(),3u);
  check_eq(tr.front_sub().arity(),0u);
  check_eq(tr[1].arity(),0u);
  check_eq(tr[2].arity(),1u);
  check_eq(tr[2][0].arity(),0u);
}

test_case(tree_append1) {
  itree tr(42);
  tr.append(1);
  tr.append(1);
  check_tree(tr,3,"42(1 1)");

  itree tr1;
  tr1.insert(tr1.end(),1);
  tr1.append(2);
  tr1[0].append(3);
  tr1.append(4);
  check_tree(tr1,4,"1(2(3) 4)");
}

test_case(tree_prepend1) {
  itree tr(-1);
  tr.prepend(42);
  check_tree(tr,2,"-1(42)");

  string front="-1(", back="42)";
  foreach(int i,11) {
    back=lexical_cast<string>(i)+" "+back;
    tr.prepend(i);
    check_tree(tr,i+3,front+back);
  }

  itree tr1(-1);
  front="-1",back="";
  itree::sub_pre_iterator it=tr1.begin_sub();
  foreach(int i,11) {
    front+="("+lexical_cast<string>(i);
    back=")"+back;
    it->prepend(i);
    ++it;
    check_tree(tr1,i+2,front+back);
  }
}

test_case(tree_append2) {
  itree tr(42);
  tr.append(itree(tr)); //need to copy to avoid infinite regress
  check_tree(tr,2,"42(42)");

  tr.append(itree(42));
  check_tree(tr,3,"42(42 42)");

  tr.append(itree(tr));
  check_tree(tr,6,"42(42 42 42(42 42))");

  tr.append(itree(tr));
  check_tree(tr,12,"42(42 42 42(42 42) 42(42 42 42(42 42)))");

  tr[0].append(tr[2]);
  check_tree(tr,15,"42(42(42(42 42)) 42 42(42 42) 42(42 42 42(42 42)))");
}

test_case(tree_prepend2) {
  itree tr(42);
  tr.prepend(itree(tr));
  check_tree(tr,2,"42(42)");

  tr.prepend(itree(42));
  check_tree(tr,3,"42(42 42)");

  tr.prepend(itree(tr));
  check_tree(tr,6,"42(42(42 42) 42 42)");

  tr.prepend(itree(tr));
  check_tree(tr,12,"42(42(42(42 42) 42 42) 42(42 42) 42 42)");

  tr[0].prepend(tr[1]);
  check_tree(tr,15,"42(42(42(42 42) 42(42 42) 42 42) 42(42 42) 42 42)");
}

test_case(tree_inplace_init) {
  itree tr1=tree_of(1);
  check_eq(tr1,itree(1));
  check_tree(tr1,1,"1");

  itree tr2=tree_of(1)(tree_of(2)(3,4),5);
  check_tree(tr2,5,"1(2(3 4) 5)");

  itree tr3=tree_of(1)(2,tree_of(3)(tree_of(4)(5)));
  check_tree(tr3,5,"1(2 3(4(5)))");
      
  itree tr4=tree_of(1)(2,3,4);
  check_tree(tr4,4,"1(2 3 4)");

  itree tr5=tree_of(1)(2,tree_of(3)(tree_of(4)(5)),6);
  check_tree(tr5,6,"1(2 3(4(5)) 6)");

  itree tr6=tree_of(1)(2,3,4,5,6,7,8,9,10,11);
  check_tree(tr6,11,"1(2 3 4 5 6 7 8 9 10 11)");
}

bool always_true() { return true; }

test_case(tree_equality) {
  vector<itree> tr;
  tr+=
      tree_of(1)(2,tree_of(3)(tree_of(4)(5)),6),
      tree_of(1)(2,tree_of(3)(tree_of(0)(5)),6),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5)),0),
      tree_of(1)(2,tree_of(3)(tree_of(4)(0)),6),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5,7)),6),
      tree_of(1)(2,tree_of(3)(tree_of(4)),5,6),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5)),6,7),
      tree_of(1)(2,3,tree_of(4)(5),6),
      itree(),
      tree_of(1);

  tr[0].append(42);
  tr[0].erase(--tr[0].end_child());
  tr[1][0].begin_child();

  foreach (int i,tr.size())
    foreach (int j,tr.size())
      if (i==j) {
        itree tmp(tr[i]);
        check_eq(tr[i],tmp);
      } else {
        check(tr[i]!=tr[j]);
      }

  check(tr[0].equal(tr[1],bind(always_true)));
  check(tr[1].equal(tr[2],bind(always_true)));
  check(tr[0].equal(tr[2],bind(always_true)));

  foreach (int i,tr.size()) {
    check(i<=3 ? //tr[0..3] have the same structure
          tr[0].equal(tr[i],bind(always_true)) :
          !tr[0].equal(tr[i],bind(always_true))); 
  }
            
  check(!tr[0].equal(tr[0],!bind(always_true)));

  itree a=tree_of(1)(tree_of(2)(3,4,5));
  itree b=tree_of(1)(tree_of(2)(tree_of(3)(4,5)));
  check(a[0][0]!=b[0][0]);
}

test_case(tree_less) {
  vector<itree> tr;
  tr+=
      itree(),
      tree_of(1),
      tree_of(1)(2,3,tree_of(4)(5),6),
      tree_of(1)(2,tree_of(3)(tree_of(4)),5,6),
      tree_of(1)(2,tree_of(3)(tree_of(0)(5)),6),
      tree_of(1)(2,tree_of(3)(tree_of(4)(0)),6),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5)),0),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5)),6),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5)),6,7),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5,7)),6);

  tr[3].append(42);
  tr[3].erase(--tr[3].end_child());
  tr[1][0].begin_child();

  foreach (int i,tr.size())
    foreach (int j,tr.size())
      if (i==j) {
        itree tmp(tr[i]);
        check(!(tr[i]<tmp));
        check(!(tmp<tr[i]));
        check(!tr[i].less(tmp,!bind(always_true)));
      } else {
        check_eq((tr[i]<tr[j]),i<j);
        check_eq(tr[i].less(tr[j],!bind(always_true)),
                 i<j && !tr[i].equal(tr[j],bind(always_true)));
      }
}

template<typename It1,typename It2>
void cr(It1 f1,It1 l1,It2 f2) {
  for (;f1!=l1;++f1,++f2)
    check_eq(*f1,*f2);
}

#define check_range(f1,l1,f2,l2)            \
{                                           \
  check_eq(distance(f1,l1),distance(f2,l2));\
  cr(f1,l1,f2);                             \
}

test_case(tree_const_iters) {
  itree tr=tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8);
  vector<int> pre_contents(count_it(1),count_it(9));
  vector<itree> pre_subtr;
  pre_subtr+=
      tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8),
      tree_of(2),
      tree_of(3)(tree_of(4)(5),6),
      tree_of(4)(5),
      tree_of(5),
      tree_of(6),
      tree_of(7),
      tree_of(8);
  vector<int> child_contents;
  child_contents+=2,3,7,8;
  vector<itree> child_subtr;
  child_subtr+=
      tree_of(2),
      tree_of(3)(tree_of(4)(5),6),
      tree_of(7),
      tree_of(8);
  vector<int> post_contents;
  post_contents+=2,5,4,6,3,7,8,1;
  vector<itree> post_subtr;
  post_subtr+=
      tree_of(2),
      tree_of(5),
      tree_of(4)(5),
      tree_of(6),
      tree_of(3)(tree_of(4)(5),6),
      tree_of(7),
      tree_of(8),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8);
  
  check_range(tr.begin(),tr.end(),
              pre_contents.begin(),pre_contents.end());
  check_range(tr.begin_sub(),tr.end_sub(),
              pre_subtr.begin(),pre_subtr.end());
  
  check_range(tr.begin_child(),tr.end_child(),
              child_contents.begin(),child_contents.end());
  check_range(tr.begin_sub_child(),tr.end_sub_child(),
              child_subtr.begin(),child_subtr.end());

  check_range(tr.begin_post(),tr.end_post(),
              post_contents.begin(),post_contents.end());
  check_range(tr.begin_sub_post(),tr.end_sub_post(),
              post_subtr.begin(),post_subtr.end());
}

test_case(tree_const_iters_backwards) {
  itree tr=tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8);
  vector<int> pre_contents(count_it(1),count_it(9));
  vector<itree> pre_subtr;
  pre_subtr+=
      tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8),
      tree_of(2),
      tree_of(3)(tree_of(4)(5),6),
      tree_of(4)(5),
      tree_of(5),
      tree_of(6),
      tree_of(7),
      tree_of(8);
  vector<int> child_contents;
  child_contents+=2,3,7,8;
  vector<itree> child_subtr;
  child_subtr+=
      tree_of(2),
      tree_of(3)(tree_of(4)(5),6),
      tree_of(7),
      tree_of(8);
  vector<int> post_contents;
  post_contents+=2,5,4,6,3,7,8,1;
  vector<itree> post_subtr;
  post_subtr+=
      tree_of(2),
      tree_of(5),
      tree_of(4)(5),
      tree_of(6),
      tree_of(3)(tree_of(4)(5),6),
      tree_of(7),
      tree_of(8),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8);

  check_range(reverse_it(tr.end()),reverse_it(tr.begin()),
              pre_contents.rbegin(),pre_contents.rend());
  check_range(reverse_it(tr.end_sub()),reverse_it(tr.begin_sub()),
              pre_subtr.rbegin(),pre_subtr.rend());
  
  check_range(reverse_it(tr.end_child()),reverse_it(tr.begin_child()),
              child_contents.rbegin(),child_contents.rend());
  check_range(reverse_it(tr.end_sub_child()),reverse_it(tr.begin_sub_child()),
              child_subtr.rbegin(),child_subtr.rend());

  check_range(reverse_it(tr.end_post()),reverse_it(tr.begin_post()),
              post_contents.rbegin(),post_contents.rend());

  check_range(reverse_it(tr.end_sub_post()),reverse_it(tr.begin_sub_post()),
              post_subtr.rbegin(),post_subtr.rend());
}

test_case(tree_mutable_iters) {
  itree tr=tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,tree_of(8)(9,10),
                      tree_of(11)(12));
  check_tree(tr,12,"1(2 3(4(5) 6) 7 8(9 10) 11(12))");
  
  std::copy(reverse_it(count_it(13)),reverse_it(count_it(1)),tr.begin());
  check_tree(tr,12,"12(11 10(9(8) 7) 6 5(4 3) 2(1))");

  std::copy(count_it(1),count_it(13),tr.begin());
  check_tree(tr,12,"1(2 3(4(5) 6) 7 8(9 10) 11(12))");

  itree tr2(42);
  *tr2.begin_sub()=tr;
  check_tree(tr2,tr.size(),lexical_cast<string>(tr));

  std::copy(count_it(100),count_it(105),tr2.begin_sub_child());
  check_tree(tr2,6,"1(100 101 102 103 104)");

  std::copy(repeat_it(tr),repeat_it(tr,5),tr2.begin_sub_child());
  check_eq(tr2.size(),61u);
  check_eq(distance(tr2.begin_sub_child(),tr2.end_sub_child()),5);
  check(equal(tr2.begin_sub_child(),tr2.end_sub_child(),repeat_it(tr)));
}

test_case(tree_accessors) {
  itree tr=tree_of(1)(2,
                      tree_of(3)(
                          tree_of(
                              4)(
                                  5),
                          6),
                      7,
                      8);
 
  check_eq(tr.root(),1);
  check_eq(tr.front(),2);
  check_eq(tr.back(),8);
  check_eq(tr.front_sub(),tree_of(2));
  check_eq(tr.back_sub(),tree_of(8));

  check_eq(tr.root_sub(),tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8));
  check_eq(tr[0],tree_of(2));
  check_eq(tr[1],tree_of(3)(tree_of(4)(5),6));
  check_eq(tr[1][0],tree_of(4)(5));
  check_eq(tr[1][0][0],tree_of(5));
  check_eq(tr[1][1],tree_of(6));
  check_eq(tr[2],tree_of(7));
  check_eq(tr[3],tree_of(8));
}

test_case(tree_insert2) {
  itree tr;

  tr.insert(tr.end(),tree_of(1)(2,3));
  check_eq(tr,tree_of(1)(2,3));

  tr.insert(tr[1].begin_child(),tree_of(4)(5,6));
  check_eq(tr,tree_of(1)(2,tree_of(3)(tree_of(4)(5,6))));
  
  tr.insert(tr[0].begin_child(),tr.begin_child(),tr.end_child());
  check_eq(tr,tree_of(1)(tree_of(2)(2,3),tree_of(3)(tree_of(4)(5,6))));

  itree tr1(42);
  itree tr2=tree_of(1)(2,3);
  tr1.insert(tr1.begin_child(),tr2.begin_sub(),tr2.end_sub());
  check_eq(tr1,tree_of(42)(tree_of(1)(2,3),
                           tree_of(2),
                           tree_of(3)));

  itree tr3(42);
  tr3.insert(tr3.begin_child(),100u,3);
  check_eq(tr3.size(),101u);
  check_eq(tr3.root(),42);
  itree::pre_iterator i=tr3.begin();
  for (int j=0;j<100;++j)
    check_eq(*++i,3);
  
  tr3.insert(tr3[0].begin_child(),10u,4);
  i=tr3[0].begin();
  for (int j=0;j<10;++j)
    check_eq(*++i,4);
  
  itree tr4(42);
  tr4.insert(tr4.begin_child(),8,tr2);
  check_eq(tr4.size(),25u);
  foreach (int i,8)
    check_eq(tr4[i],tr2);
}

test_case(tree_append3) {
  itree tr(42);
  vector<int> toadd(count_it(1),count_it(101));
  tr.append(toadd.begin(),toadd.end());
  check_eq(tr.size(),101u);
  check_eq(tr.root(),42);
  check(equal(tr.begin_child(),tr.end_child(),toadd.begin()));

  itree tr1(42);
  tr1.append(-1);
  tr1[0].append(10u,99);
  check_eq(tr1.size(),12u);
  itree::pre_iterator i=++++tr1.begin();
  itree::sub_pre_iterator j=++++tr1.begin_sub();
  while (i!=tr1.end()) {
    check_eq(*i,99);
    check_eq(*j,tree_of(99));
    ++i,++j;
  }
}

test_case(tree_prepend3) {
  itree tr(42);
  vector<int> toadd(count_it(1),count_it(101));
  tr.prepend(toadd.begin(),toadd.end());
  check_eq(tr.size(),101u);
  check_eq(tr.root(),42);
  check(equal(tr.begin_child(),tr.end_child(),toadd.begin()));

  itree tr1(42);
  tr1.prepend(-1);
  tr1.prepend(10u,99);
  check_eq(tr1.size(),12u);
  itree::pre_iterator i=++tr1.begin();
  itree::sub_pre_iterator j=++tr1.begin_sub();
  while (i!=prior(tr1.end())) {
    check_eq(*i,99);
    check_eq(*j,tree_of(99));
    ++i,++j;
  }
}

test_case(tree_append4) {
  itree tr(42);
  tr.append(0);
  tr.append(4,tree_of(1)(2,3));
  check_tree(tr,14,"42(0 1(2 3) 1(2 3) 1(2 3) 1(2 3))");
}

test_case(tree_prepend4) {
  itree tr(42);
  tr.prepend(0);
  tr.prepend(4,tree_of(1)(2,3));
  check_tree(tr,14,"42(1(2 3) 1(2 3) 1(2 3) 1(2 3) 0)");
}

test_case(tree_append5) {
  itree tr=tree_of(0)(0,0);
  vector<itree> foo;
  foo+=tree_of(1)(2,3),tree_of(4),tree_of(5)(tree_of(6)(7));
  
  tr.append(foo.begin(),foo.end());
  check_tree(tr,10,"0(0 0 1(2 3) 4 5(6(7)))");

  tr[2].append(2,foo[0]);
  check_tree(tr,16,"0(0 0 1(2 3 1(2 3) 1(2 3)) 4 5(6(7)))");
}

test_case(tree_prepend5) {
  itree tr=tree_of(0)(0,0);
  vector<itree> foo;
  foo+=tree_of(1)(2,3),tree_of(4),tree_of(5)(tree_of(6)(7));
  
  tr.prepend(foo.begin(),foo.end());
  check_tree(tr,10,"0(1(2 3) 4 5(6(7)) 0 0)");

  tr[2].prepend(2,foo[0]);
  check_tree(tr,16,"0(1(2 3) 4 5(1(2 3) 1(2 3) 6(7)) 0 0)");
}

test_case(tree_insert_above) {
  itree tr(42);
  tr.insert_above(tr.begin(),41);
  check_tree(tr,2,"41(42)");

  tr.insert_above(tr[0].begin(),40);
  check_tree(tr,3,"41(40(42))");

  tr[0].append(43);
  tr.insert_above(tr[0].begin_child(),39);
  check_tree(tr,5,"41(40(39(42) 43))");
}

test_case(tree_insert_below) {
  itree tr(42);

  tr.insert_below(tr.begin(),41);
  check_tree(tr,2,"42(41)");

  tr.insert_below(tr.begin(),40);
  check_tree(tr,3,"42(40(41))");

  tr[0].append(43);
  tr.insert_below(tr[0].begin(),39);
  check_tree(tr,5,"42(40(39(41 43)))");
}

test_case(tree_insert_flatten) {
  itree tr=tree_of(0)(tree_of(1)(2,tree_of(3)(tree_of(4)(5,6))));
  
  tr.flatten(tr.begin_child());
  check_tree(tr,7,"0(1 2 3(4(5 6)))");

  itree tmp(tr);

  tr.flatten(tr[2].begin());
  check_tree(tr,7,"0(1 2 3 4(5 6))");

  tmp.flatten(tmp[2].begin_child());
  check_tree(tmp,7,"0(1 2 3(4 5 6))");

  tmp.flatten(tmp[2].begin());
  check_tree(tmp,7,"0(1 2 3 4 5 6)");
}

test_case(tree_erase1) {
  itree tr=tree_of(1)(tree_of(2)(3,4,42),
                      tree_of(5)(tree_of(6)(7,8)),
                      tree_of(9)(tree_of(10)(11),12));
  check_tree(tr,13,"1(2(3 4 42) 5(6(7 8)) 9(10(11) 12))");

  tr.erase(tr[1][0]);
  check_tree(tr,10,"1(2(3 4 42) 5 9(10(11) 12))");

  tr.erase(tr[2][1]);
  check_tree(tr,9,"1(2(3 4 42) 5 9(10(11)))");

  tr.erase(tr[2][0][0]);
  check_tree(tr,8,"1(2(3 4 42) 5 9(10))");

  tr.erase(tr[0][1]);
  check_tree(tr,7,"1(2(3 42) 5 9(10))");

  tr.erase(tr[0][0]);
  check_tree(tr,6,"1(2(42) 5 9(10))");

  tr.erase(tr.begin());
  check_tree(tr,0,"");
}

test_case(tree_erase2) {
  itree tr=tree_of(1)(tree_of(5)(tree_of(6)(7,8)),
                      tree_of(9)(tree_of(10)(11),12),
                      tree_of(2)(3,4,42));
  check_tree(tr,13,"1(5(6(7 8)) 9(10(11) 12) 2(3 4 42))");

  tr.erase(tr[0].begin_child(),tr[0].end_child());
  check_tree(tr,10,"1(5 9(10(11) 12) 2(3 4 42))");

  tr.erase(tr[0].begin_child(),tr[0].end_child());
  check_tree(tr,10,"1(5 9(10(11) 12) 2(3 4 42))");

  tr.erase(++tr.begin_child(),tr.end_child());
  check_tree(tr,2,"1(5)");

  tr.erase(tr.begin_child(),tr.end_child());
  check_tree(tr,1,"1");
}

test_case(tree_prune) {
  itree tr=tree_of(1)(tree_of(2)(3,4),
                      tree_of(5)(tree_of(6)(7,8)),
                      tree_of(9)(tree_of(10)(11),12));
  check_tree(tr,12,"1(2(3 4) 5(6(7 8)) 9(10(11) 12))");

  tr[0].prune();
  check_tree(tr,10,"1(2 5(6(7 8)) 9(10(11) 12))");

  tr[2][0].prune();
  check_tree(tr,9,"1(2 5(6(7 8)) 9(10 12))");

  tr.prune();
  check_tree(tr,1,"1");
}

test_case(tree_clear) {
  itree tr(42);

  itree::child_iterator ci=tr.begin_child();

  tr.clear();
  check_tree(tr,0,"");

  tr.clear();
  check_tree(tr,0,"");

  tr.insert(tr.end(),1);
  tr.clear();
  check_tree(tr,0,"");
  
  tr.insert(tr.end(),1);
  tr.append(20u,tree_of(1)(2,3,tree_of(4)(5)));
  tr[0].append(5u,itree(tr));
  tr.clear();
  check_tree(tr,0,"");
}

test_case(tree_splice1) {
  itree tr1=tree_of(1)(2,3),tr2=tree_of(4),tr3=tree_of(5)(6,tree_of(7)(8));

  tr1.splice(tr1[0].begin(),tr2);
  check_tree(tr2,0,"");
  check_tree(tr1,4,"1(4 2 3)");

  tr1.splice(tr1.end_child(),tr1[0]);
  check_tree(tr1,4,"1(2 3 4)");

  tr1.splice(tr1[1].begin(),tr1.back_sub());
  check_tree(tr1,4,"1(2 4 3)");
  
  tr1.splice(tr1.end_child(),tr1[1]);
  check_tree(tr1,4,"1(2 3 4)");

  tr1.splice(++tr1.begin_child(),tr3[1]);
  check_tree(tr1,6,"1(2 7(8) 3 4)");
  check_tree(tr3,2,"5(6)");

  tr1.splice(tr3.end_child(),tr1[1]);
  check_tree(tr1,4,"1(2 3 4)");
  check_tree(tr3,4,"5(6 7(8))");

  tr1.splice(--tr1.end(),tr3);
  check_tree(tr1,8,"1(2 3 5(6 7(8)) 4)");
  check_tree(tr3,0,"");

  itree tr4=tree_of(1)(2);
  tr1.splice(tr1.end_child(),tr4.begin_child());
  check_tree(tr4,1,"1");
  check_tree(tr1,9,"1(2 3 5(6 7(8)) 4 2)");
  

  tr4.splice(tr4.end_child(),tr1);
  check_tree(tr1,0,"");
  check_tree(tr4,10,"1(1(2 3 5(6 7(8)) 4 2))");
}

test_case(tree_splice2) {
  itree tr1=tree_of(1)(2,3),tr2=tree_of(4),tr3=tree_of(5)(6,tree_of(7)(8));

  tr1.splice(tr1[0].begin(),tr2.begin_child(),tr2.end_child());
  check_tree(tr1,3,"1(2 3)");
  check_tree(tr2,1,"4");

  tr1.splice(tr1.end_child(),tr3.begin_child(),tr3.end_child());
  check_tree(tr1,6,"1(2 3 6 7(8))");
  check_tree(tr3,1,"5");

  tr3.splice(tr3.end_child(),++tr1.begin_child(),--tr1.end_child());
  check_tree(tr1,4,"1(2 7(8))");
  check_tree(tr3,3,"5(3 6)");
}

test_case(tree_swap) {
  vector<itree> foo,goo;
  foo+=tree_of(1)(2,3),tree_of(4),tree_of(5)(tree_of(6)(7)),itree();
  goo=foo;
  for (unsigned int i=0;i<foo.size();++i)
    for (unsigned int j=0;j<foo.size();++j) {
      foo[i].swap(foo[j]);
      check_eq(foo[i],goo[j]);
      check_eq(foo[j],goo[i]);

      foo[i].swap(foo[j]);
      check_eq(foo[i],goo[i]);
      check_eq(foo[j],goo[j]);
    }
  std::swap(foo[0],foo[1]);
  std::swap(foo[2],foo[3]);
}

test_case(tree_parent) {
  itree tr=tree_of(1)(2,3,tree_of(4)(5,6));

  check(parent(tr.begin())==tr.end());

  check(parent(tr[0].begin())==tr.begin());
  check(parent(tr[1].begin())==tr.begin());
  check(parent(tr[2].begin())==tr.begin());

  check(parent(tr[2][0].begin())==tr[2].begin());
  check(parent(tr[2][1].begin())==tr[2].begin());
}

#define check_parse(src,goal)                      \
  { stringstream ss;                               \
    tree<string> tmpXXX;                           \
    ss << src;                                     \
    ss >> tmpXXX;                                  \
    check_eq(lexical_cast<string>(tmpXXX),goal);   \
  }
#define check_parse2(src,goal,goal_tr)             \
  { stringstream ss;                               \
    tree<string> tmpXXX;                           \
    ss << src;                                     \
    ss >> tmpXXX;                                  \
    check_eq(lexical_cast<string>(tmpXXX),goal);   \
    check_eq(tmpXXX,goal_tr);                      \
  }

test_case(tree_io) {
  tree<string> tr=tree_of(string("1"))(tree_of(string("2"))(string("2.5"),
                                                            string("2.8")),
                                       string("3"),
                                       tree_of(string("4"))(string("5"),
                                                            string("6")));
  string s="1(2(2.5 2.8) 3 4(5 6))";
  check_tree(tr,8,s);

  check_parse2(s,s,tr);
}

test_case(tree_io_empty) {
  tree<string> tr;
  string s="";
  check_tree(tr,0,s);

  check_parse2(s,s,tr);
}

test_case(tree_out_sexpr) {
  itree tr=tree_of(1)(tree_of(5)(tree_of(6)(7,8)),
                      tree_of(9)(tree_of(10)(11),12),
                      tree_of(2)(3,4,42));
    check_eq(lexical_cast<string>(sexpr_format(tr)),
             "(1 (5 (6 7 8)) (9 (10 11) 12) (2 3 4 42))");
}

test_case(tree_in_sexpr) {
  tree<string> tr=tree_of(string("1"))(tree_of(string("2"))(string("25"),
                                                            string("28")),
                                       string("3"),
                                       tree_of(string("4"))(string("5"),
                                                            string("6")));
  itree tr2=tree_of(1)(tree_of(2)(25,28),3,tree_of(4)(5,6));

  const string s="(1 (2 25 28) 3 (4 5 6))";
  stringstream ss;
  tree<string> tmpXXX;
  ss << s;
  ss >> sexpr_format(tmpXXX);
  check_eq(lexical_cast<string>(sexpr_format(tmpXXX)),s);
  check_eq(tmpXXX,tr);

  itree tmpYYY;
  ss << s;
  ss >> sexpr_format(tmpYYY);
  check_eq(tmpYYY,tr2);
}

struct Simple {
  static vector<int> record;
  Simple(int n) : _n(n) { }
  ~Simple() { ++record[_n]; }
  int _n;
};

vector<int> Simple::record;

test_case(tree_node_destructor) {
  Simple::record=vector<int>(8,0);
  tree<Simple> tree_simple = 
      tree_of(Simple(7))(tree_of(Simple(0))(Simple(1),Simple(2)),Simple(3),
                         tree_of(Simple(4))(Simple(5),Simple(6)));
  vector<int> tmp=Simple::record;
  tree_simple.clear();
  for (int i=0;i<8;++i)
    check_eq(tmp[i]+1,Simple::record[i]);
}

/// EOF ///
