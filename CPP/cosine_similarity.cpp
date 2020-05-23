// cosine_similarity.cpp : Diese Datei enthält die Funktion "main". Hier beginnt und endet die Ausführung des Programms.
//

#include <iostream>
#include <string>
#include "cosine_similarity.h"
#include <vector>
#include <math.h>
#include <algorithm>    // std::find

/// <summary>
/// 
/// </summary>
/// <param name="a"></param>
/// <param name="b"></param>
/// <param name="lengthA"> length of array a </param>
/// <param name="lengthB"> length of array b </param>
/// <returns></returns>
double cosine_similarity(std::string* a, std::string* b, int lengthA, int lengthB) {
    // Generating vectors
    std::vector<bool> a_count, b_count;
    std::vector<std::string> bsearch(b, b + lengthB);
    std::vector<std::string> helper;
    for (int i = 0; i < lengthA; i++) {
        a_count.push_back(true);
        bool found = std::find(bsearch.begin(), bsearch.end(), a[i]) != bsearch.end();
        b_count.push_back(found);
        if (found)
            helper.push_back(a[i]);
    }

    for (int i = 0; i < lengthB; i++) {
        if (!(std::find(helper.begin(), helper.end(), a[i]) != helper.end())) {
            a_count.push_back(false);
            b_count.push_back(true);
        }
    }

    // cosine calc
    int length = a_count.size();
    double nom = 0.0, denomA = 0.0, denomB = 0.0;
    for (unsigned int i = 0; i < length; ++i) {
        nom += a_count[i] & b_count[i];
        denomA += a_count[i] & a_count[i];
        denomB += b_count[i] & b_count[i];
    }
    return nom / (sqrt(denomA) * sqrt(denomB));
}
