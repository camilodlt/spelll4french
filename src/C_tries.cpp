#include <Rcpp.h>
#include <boost/algorithm/string.hpp>
#include <string>
#include <vector>
#include <typeinfo>
using namespace Rcpp;

// [[Rcpp::depends(BH)]]
// [[Rcpp::export]]
List cutcut(Rcpp::String x, CharacterVector letters){
std::string text = std::string(x);
std::vector<std::string> results;
boost::split(results, text, [](char c){return c == ' ';});
std::string temp =results[0];
std::vector<std::string> TempHolder;
//std::vector<std::vector<std::string>> results_holder;
Rcpp::List list_holder(1);
for(int i = 0; i < temp.size()+1; i++){
  for(int l = 0; l < letters.size(); l++){
    std::string to_add = std::string(letters[l]);
  if(i == 0) {
      TempHolder.insert(TempHolder.end(),to_add+temp);
    }
   else if(i < temp.size()) { // one before last element
      std::string start(temp.begin(), temp.begin()+i);
      std::string end(temp.begin()+i,temp.end());
      TempHolder.insert(TempHolder.end(),start+to_add+end);
  } else if(i == temp.size()) { // last element
     std::string start(temp.begin(), temp.end());
     TempHolder.insert(TempHolder.end(),start+to_add);
   }

  //results_holder.insert(results_holder.end(),TempHolder);
  //list_holder.push Rcpp::wrap(TempHolder);
  //Rcout << TempHolder << "\n";
  }
  }
Rcpp::StringVector TempString(TempHolder.size());
TempString = TempHolder;
list_holder[0]=TempString;

//return(Rcpp::wrap(TempString));
return(list_holder);

}

