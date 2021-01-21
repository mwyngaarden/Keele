#include <array>
#include <bitset>
#include <iostream>
#include <thread>
#include <vector>
#include <cstring>
#include <cstdlib>
#include "gen.h"
#include "hash.h"
#include "perft.h"
#include "piece.h"
#include "pos.h"
#include "search.h"
#include "square.h"
#include "string.h"
#include "types.h"
#include "uci.h"
using namespace std;

int main(int argc, char *argv[])
{
	uci_init();
	piece_init();
	gen_init();
	hash_init();
	uci_init();

	if (argc >= 2) {
		if (strcmp(argv[1], "-p") == 0)
			perft_validate(argc - 2, argv + 2);
		else if (strcmp(argv[1], "-h") == 0)
			hash_validate(argc - 2, argv + 2);

		return EXIT_SUCCESS;
	}

	/*
    string s1 = "\t\nHello,    World!\t";
    string s2 = "\n\nHello,  World!\r\n\v";
    string s3 = "\tHello,\t\n\r\v World!\t";

    cout << "s1(" << s1 << ")" << endl;
    cout << "s2(" << s2 << ")" << endl;
    cout << "s3(" << s3 << ")" << endl;

    cout << "s1(" << TrimBegin(s1) << ")" << endl;
    cout << "s2(" << TrimBegin(s2) << ")" << endl;
    cout << "s3(" << TrimBegin(s3) << ")" << endl;

    cout << "s1(" << TrimEnd(s1) << ")" << endl;
    cout << "s2(" << TrimEnd(s2) << ")" << endl;
    cout << "s3(" << TrimEnd(s3) << ")" << endl;

    cout << "s1(" << Trim(s1) << ")" << endl;
    cout << "s2(" << Trim(s2) << ")" << endl;
    cout << "s3(" << Trim(s3) << ")" << endl;

    return EXIT_SUCCESS;
	*/

    //setvbuf(stdin, nullptr, _IONBF, 0);
    //setvbuf(stdout, nullptr, _IONBF, 0);

	// tmp
	
	thread search_thread(search_init);
		
	uci_loop();
		
	search_thread.join();

	cerr << "Exiting!" << endl;

	return EXIT_SUCCESS;
}


