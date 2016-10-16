#include <Rcpp.h>
#include <queue>
#include <algorithm>

using namespace Rcpp;
using namespace std;

template<class T1, class T2>
static void extract_second(const vector<pair<T1, T2> >& v, vector<T1>& v2) {
    v2.resize(v.size()*2-2);
    for (size_t i = 0; i < v.size()-1; i++) {
        v2[i*2] = v[i].second + 1;
        v2[i*2+1] = floor((v[i].first)*10000)/10000.0;
    }
}

bool compare(const pair<float,int>&i, const pair<float,int>&j)
{
        return i.first < j.first;
}
// [[Rcpp::export]]
DoubleVector top_i_pq(NumericVector v, int n)
{
    n += 1;
    typedef pair<double, int> Elt;
    priority_queue< Elt, vector<Elt>, less<Elt> > pq;
    vector<Elt> topn;
    vector<double> result;
    for (int i = 0; i != v.size(); ++i) {
        if (pq.size() < n)
            pq.push(Elt(v[i], i));
        else {
            Elt elt = Elt(v[i], i);
            if (pq.top() > elt) {
                pq.pop();
                pq.push(elt);
                }
            }
            }

    topn.reserve(pq.size());
    result.reserve(pq.size()*2);
    while (!pq.empty()) {
        topn.push_back(pq.top());
        pq.pop();
    }
    
    std::sort(topn.begin(), topn.end(), compare);
    extract_second(topn, result);

    return wrap(result);
}
