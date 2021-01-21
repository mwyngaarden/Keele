#include <iostream>
#include <sstream>
#include <string>
#include <thread>
#include "string.h"
#include "uci.h"
#include "util.h"
using namespace std;

static void uci_parse   (const string& s);
static void uci_send    (const string& s);

void uci_init()
{
}

void uci_loop()
{
	string line;

	while (getline(cin, line)) {
		string token;
		
		istringstream iss(line);

		iss >> skipws >> token;

		if (token == "quit")
			break;
	}
}

void uci_parse(const string s)
{
    // tx ends is \n
    // rx ends in \n
    
    // isready
    // isready ok

    // FIXME: This does not strip arbitrary white space between tokens, which
    // is allowed, according to the UCI protocol

    // s = TrimEnd(s);

    // RX single token commands:
    // 
    // isready
    // uci
    // ucinewgame
    // stop
    // ponderhit
    // quit
    
    // TX single token responses:
    // 
    // uciok
    // readyok

    // TX simple token responses:
    //
    // copyprotection
    // copyprotection checking
    // copyprotection ok
    //
    // register
    // registration checking
    // registration ok

    if (s == "uci") {
        //uci_send("id " KEELE_VERSION);
        //uci_send("id " KEELE_AUTHOR);
    }

}
