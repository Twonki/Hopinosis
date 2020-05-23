// cosine_similarity.cpp : Diese Datei enthält die Funktion "main". Hier beginnt und endet die Ausführung des Programms.
//

#include <iostream>
#include <string>
#include "cosine_similarity.h"
#include <vector>

int main()
{
    std::cout << "Hello World!\n";

    std::vector<std::string> av = { "Hello", "was", "geht", "bei", "dir" };
    std::vector<std::string> bv = { "Tachchen", "was", "geht", "bei", "dir", "Digga" };
    
    std::string* a = &av[0];
    std::string* b = &bv[0];

    std::cout << cosine_similarity(a, b, av.size(), bv.size());
}


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

// Programm ausführen: STRG+F5 oder Menüeintrag "Debuggen" > "Starten ohne Debuggen starten"
// Programm debuggen: F5 oder "Debuggen" > Menü "Debuggen starten"

// Tipps für den Einstieg: 
//   1. Verwenden Sie das Projektmappen-Explorer-Fenster zum Hinzufügen/Verwalten von Dateien.
//   2. Verwenden Sie das Team Explorer-Fenster zum Herstellen einer Verbindung mit der Quellcodeverwaltung.
//   3. Verwenden Sie das Ausgabefenster, um die Buildausgabe und andere Nachrichten anzuzeigen.
//   4. Verwenden Sie das Fenster "Fehlerliste", um Fehler anzuzeigen.
//   5. Wechseln Sie zu "Projekt" > "Neues Element hinzufügen", um neue Codedateien zu erstellen, bzw. zu "Projekt" > "Vorhandenes Element hinzufügen", um dem Projekt vorhandene Codedateien hinzuzufügen.
//   6. Um dieses Projekt später erneut zu öffnen, wechseln Sie zu "Datei" > "Öffnen" > "Projekt", und wählen Sie die SLN-Datei aus.
