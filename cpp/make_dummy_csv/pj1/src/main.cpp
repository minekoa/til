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
#include <unordered_set>
#include <map>
#include <memory>
#include <climits>

std::vector<char> charset_email()
{
    // メールアドレスのローカル部 用文字セット
    // 使用できる文字のうち、単純なランダムの組で規約違反にならないものをチョイス
    // ( . や、 quoted string は除外した)
   return std::vector<char>( 
    {
        '0','1','2','3','4','5','6','7','8','9',
        'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
        'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
        '!','#','$','%','&','\'','*','+','-','/','=', '?', '^', '_', '`', '{', '|', '}', '~'
    }
        );
};

std::vector<char> charset_loginid()
{
    // ログインID 用文字セット
    return std::vector<char>( 
    {
        'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'
    }
        );
};

std::vector<char> charset_userdata()
{
    // ユーザーデータ 用文字セット
    return std::vector<char>( 
    {
        '0','1','2','3','4','5','6','7','8','9',
        'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'
    }
        );
};

std::vector<int> codeset_smtp_response()
{
    // SMTP 応答コード
    // RFC 5321 より
    return std::vector<int>(
    { 221,214,220,221,250,251,252,
      354,
      421,450,451,452,
      500,501,502,503,504,550,551,552,553,554
    }
        );
};

std::vector<int> month_days_list ()
{
    return std::vector<int>(
    { // 1   2   3   4   5   6   7   8   9  10  11  12
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, // 1999
        31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, // 2000 (うるう年)
    }
        );
}


/**
 * 渡されたアイテム(T型)の組からランダムに一つを選択する
 */
template<typename T>
class RandGen {
private:
    std::vector<T> m_choice;
    std::uniform_int_distribution<> m_dist;
    std::default_random_engine      m_rng;

public:
    RandGen(const std::vector<T> choice)
        : m_choice(choice)
        , m_dist( std::uniform_int_distribution<>(0, choice.size() -1) )
        , m_rng( (std::random_device{}()) )
    {
        ;
    }

    T get()
    {
        return m_choice[ m_dist(m_rng) ];
    }
};

/**
 * 渡された文字セットから、ランダムな文字列を生成する
 */
class RandStringGen {
private:
    std::vector<char> m_charset;
    std::uniform_int_distribution<> m_dist;
    std::default_random_engine      m_rng;

public:
    RandStringGen(const std::vector<char> charset)
        : m_charset(charset)
        , m_dist( std::uniform_int_distribution<>(0, charset.size() -1) )
        , m_rng( (std::random_device{}()) )
    {
        ;
    }

    char getChar()
    {
        return m_charset[ m_dist(m_rng) ];
    }

    std::string getString(size_t length)
    {
        std::string str(length,0);
        std::generate_n( str.begin(), length, [&](){return this->getChar();} );
        return str;
    }
};


/**
 * 渡された文字セットから、ランダムな文字列を生成する。
 * 生成された文字列は互いにユニークとなる
 *
 * note:
 *    uniq 判定に undorderd_set (hash map) を利用しているため、
 *    生成すればするほど、メモリを圧迫する
 *
 * note: 使用上の注意として
 *    * ユニークなものがもう作れない（これ以上組み合わせがない）場合でも停止しない
 *    * また、逼迫してくるとID生成リトライしっぱなしになり、パフォーマンスが悪化する
 */
class RandUniqStringGen {
private:
    std::vector<char> m_charset;
    std::uniform_int_distribution<> m_dist;
    std::default_random_engine      m_rng;
    std::unordered_set<std::string> m_set;  // note; std::set よりは若干メモリ効率/実行性能がよかった

public:
    RandUniqStringGen(const std::vector<char> charset)
        : m_charset(charset)
        , m_dist( std::uniform_int_distribution<>(0, charset.size() -1) )
        , m_rng( (std::random_device{}()) )
        , m_set()
    {
        ;
    }

private:
    char getChar()
    {
        return m_charset[ m_dist(m_rng) ];
    }

public:
    std::string getString(size_t length)
    {
        while (1) {
            std::string str(length,0);
            std::generate_n( str.begin(), length, [&](){return this->getChar();} );

            auto retpair = m_set.insert(str);

            if (retpair.second == true) {
                return str;
            }

            // std::cout << "duplicate!" << str << std::endl; //dbg
        }
    }
};


/**
 * ユニークなログインID(6文字/英小文字) を作ってファイルに保存する
 */
void createUniqLoginIdFile( const std::string & filename, long lines_num )
{
    std::ofstream f;
    f.open(filename,std::ios::trunc);

    auto loginId = RandUniqStringGen( charset_loginid() );

    for (int i = 0; i < lines_num; i++) {
        f << loginId.getString(6) << std::endl;
    }

    f.close();
}

/**
 * ユニークなメールアドレス を作ってファイルに保存する
 *
 * * ローカル部は8文字固定とした（手抜き）
 * * ドメインパートは
 *      * @hoge.co.jp    .. 30%
 *      * @xyzzy.jp      .. 20%
 *      * @foobarbuz.jp  .. 10%
 *      * その他は弱小ドメイン（ランダム生成）それぞれに10〜100件程度入るようにする
 */
void createUniqMailAddressFile( const std::string & filename, long lines_num )
{
    std::ofstream f;
    f.open(filename,std::ios::trunc);

    auto addressLocal = RandUniqStringGen( charset_email() );

    // Mail Address (Domain)
    auto small_dom_num = static_cast<int>(lines_num * 0.4 / 60) + 1; // 100〜10件とあるので

    std::vector<std::string> small_doms;
    {
        auto domname = RandUniqStringGen( charset_userdata() );
        auto tld     = RandGen<std::string>({ ".com", ".net", ".ne.jp", ".co.jp" });

        for (int i = 0; i < small_dom_num; i++) {
            std::ostringstream oss;
            oss << "@" << domname.getString(6) << tld.get();
            small_doms.push_back(oss.str());
            oss.clear();
        }
    }

    std::uniform_int_distribution<> sdom_dist(0,small_doms.size()-1);

    std::default_random_engine      rng(std::random_device{}());
    std::uniform_int_distribution<> dom_dist(0, 100 -1);
    auto domain_rand = [&dom_dist, &rng, small_doms, &sdom_dist ]() {
                           auto c = dom_dist(rng);

                           if      (c < 30) { return std::string("@hoge.co.jp"); }
                           else if (c < 50) { return std::string("@xyzzy.jp"); }
                           else if (c < 60) { return std::string("@foobarbuz.jp"); }
                           else {
                               return small_doms[ sdom_dist(rng) ];
                           }
                       };



    for (int i = 0; i < lines_num; i++) {
        f  << addressLocal.getString(8) << domain_rand() << std::endl;
    }

    f.close();
}


int main (int argc, char const * const argv[]) {

    std::vector<std::string> args{ argv, argv + argc };

    if (argc < 2) {
        std::cerr << "no argument" << std::endl;
        exit(-1);
    }
    long lines_num;
    try {
        lines_num = std::stol( args[1] );
    }
    catch (std::invalid_argument e){
        std::cerr << "invalid argument" << std::endl;
        exit(-1);
    }
    catch (std::out_of_range e){
        std::cerr << "invalid argument (out of range)" << std::endl;
        exit(-1);
    }
    if (lines_num > 398915776) { // 26 ** 6 ; これ以上はユニークなlogin IDが原理上作れない
        std::cerr << "invalid argument (too may rows)" << std::endl;

        exit(-1);
    }



    // ユニークなカラムは一度ファイルに保存（省メモリのため）-------------------------
    createUniqMailAddressFile("mailaddr.tmp", lines_num);     // Mail Address
    createUniqLoginIdFile( "loginid.tmp", lines_num); // Login ID


    // 非ユニークなカラム用に ランダムジェネレータを用意 -----------------------------

    // SMTP Response Code
    auto smtpResp     = RandGen<int>( codeset_smtp_response() );

    // DateTime
    std::default_random_engine      rng(std::random_device{}());     // 乱数生成器 (共用)

    //  .. (date ∈ {1999/01/01.. 2000/12/31} )
    const auto date_set = month_days_list();
    std::vector<int> date_range_guide;
    int date_sum;
    for ( auto d : date_set ) {
        date_sum += d;
        date_range_guide.push_back( date_sum );
    }    
    std::uniform_int_distribution<> dt_dist(0, date_sum -1 );
    auto randdatestring = [ &dt_dist, &rng, &date_range_guide ] () {
                              auto days = dt_dist(rng);

                              unsigned int mon = 0;
                              for (; mon <  date_range_guide.size(); mon++ ) {
                                  if (days < date_range_guide[mon]) break;
                              }

                              std::ostringstream oss;
                              oss << ( mon < 12 ? "1999" : "2001") << "/"
                                  << std::setw(2) << std::setfill('0') << ((mon % 12) + 1) << "/"
                                  << std::setw(2) << std::setfill('0') << days - (mon == 0 ? 0 : date_range_guide[mon-1]);
                              return oss.str();
                          };

    //  .. (time)
    std::uniform_int_distribution<> tm_dist(0, (24 * 60 * 60) -1 );
    auto randtimestring = [ &tm_dist, &rng ](){
                              auto sec = tm_dist(rng);
                              std::ostringstream oss;
                              oss << std::setw(2) << std::setfill('0') << ((sec / 60 / 60) % 24) << ":"
                                  << std::setw(2) << std::setfill('0') << ((sec / 60) % 60)      << ":"
                                  << std::setw(2) << std::setfill('0') << (sec % (60)) ;
                              return oss.str();
                          };

    // User Data
    auto userData = RandStringGen( charset_userdata() );


    // ファイルに出力 ----------------------------------------------------------------

    std::ofstream log;
    log.open("log.csv",std::ios::trunc);


    std::ifstream mailAddrF("mailaddr.tmp");
    std::ifstream loginIdF("loginid.tmp");


    for (int i = 0; i < lines_num; i++) {
        std::string loginid;
        std::string mailaddr;

        std::getline(loginIdF, loginid);
        std::getline(mailAddrF, mailaddr);

        log << i << ","
            << mailaddr << ","
            << smtpResp.get() << ","
            << randdatestring() << " " << randtimestring() << ","
            << loginid << ","
            << userData.getString(100) << ","
            << std::endl;
    }
    log.close();

    return 0;
}






