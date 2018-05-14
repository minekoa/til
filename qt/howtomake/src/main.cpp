#include <QtGui>

int main (int argc, char *argv[])
{
    QApplication app(argc, argv);

    QPushButton hello_btn ( "hello, world" );
    hello_btn.resize( 100, 30 );

    hello_btn.show();

    app.exec();
}
