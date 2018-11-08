#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <vector>
#include <string>
#include <random>
#include <functional>
#include <algorithm>
#include <set>
#include <unordered_map>


std::vector<std::string> split( const std::string& str, char sepchar)
{
    std::stringstream ss{str};

    std::vector<std::string> ret;

    std::string buf;
    while (std::getline(ss, buf, sepchar)) {
        ret.push_back(buf);
    }

    return ret;
}


int main (int argc, char const * const argv[]) {

    std::vector<std::string> args{ argv, argv + argc };

    if (argc < 3) {
        std::cerr << "no argument" << std::endl;
        exit(-1);
    }

    std::string filepath1 = args[1];
    std::string filepath2 = args[2];

    // file2 に対して、login_id -> seekoffset のマップを作る
    std::unordered_map<std::string, std::streamoff> f2map;
    {
        std::ifstream iss(filepath2);

        std::streamoff offset =0;

        std::string linebuf;
        while (std::getline(iss, linebuf)) {
            auto row = split(linebuf, ',');

            if (5 < row.size()) {
                std::string loginid = row.at(4);
                f2map.insert( std::make_pair(loginid, offset) );
            }

            offset += linebuf.size() +1; // 取り除かれた \n の分
        }

        iss.close();
    }

    // file1 を舐めながら loginid一致行を出力

    std::ofstream outf;
    outf.open("out.txt",std::ios::trunc);

    std::ifstream f1ss(filepath1);
    std::ifstream f2ss(filepath2);

    std::string linebuf;
    while (std::getline(f1ss, linebuf)) {
        auto row = split(linebuf, ',');

        if (5 < row.size()) {
            std::string loginid = row.at(4);

            auto it = f2map.find(loginid);
            if (it != f2map.end()) {
                f2ss.seekg(it->second, f2ss.beg);
                std::string f2line;
                std::getline(f2ss, f2line);


                outf << linebuf << std::endl
                     << f2line << std::endl
                     << std::endl;

            //     std::cout << "!";
            // }
            // else {
            //     std::cout << ".";
            }
        }
    }

    outf.close();
    f1ss.close();
    f2ss.close();

    return 0;
}

