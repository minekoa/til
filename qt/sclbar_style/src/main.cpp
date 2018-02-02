#include <QtGui/QApplication>

#include <QtGui/QWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QScrollArea>
#include <QtGui/QScrollBar>
#include <QtGui/QPushButton>
#include <QtGui/QLabel>

int main (int argc, char *argv[])
{
    QApplication app(argc, argv);
    QWidget* parent = new QWidget();

    QScrollArea *sclArea = new QScrollArea(parent);
    sclArea->resize(200, 100);
    sclArea->move( 10 , 20 );
    sclArea->setLayout(new QVBoxLayout());
//    sclArea->setStyleSheet("background:transparent; border:none;");

    QWidget* contents = new QWidget();
    contents->setLayout(new QVBoxLayout());
    for (int i =0 ; i < 10 ; i++) {
        contents->layout()->addWidget( new QLabel( QString("aaabbbcccdddeeefffggghhhiiijjjkkklll") + QString::number(i) ) );
    }
    sclArea->setWidget(contents);



    parent->show();

    app.exec();
}


