#include <QtGui/QApplication>

#include <QtGui/QWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QScrollArea>
#include <QtGui/QScrollBar>
#include <QtGui/QPushButton>
#include <QtGui/QLabel>
#include <QtCore/QFile>

int main (int argc, char *argv[])
{
    QApplication app(argc, argv);
    QWidget* parent = new QWidget();

    QScrollArea *sclArea = new QScrollArea(parent);
    sclArea->resize(200, 150);
    sclArea->move( 10 , 20 );
    sclArea->setLayout(new QVBoxLayout());

//    sclArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
//    sclArea->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

    // StyleSheet (QSS)
    {
        QString style_file_path = app.applicationDirPath() + "/style.qss";
        if (QFile::exists(style_file_path)) {
            QFile style_file(style_file_path);
            style_file.open(QFile::ReadOnly);
            app.setStyleSheet( QLatin1String(style_file.readAll()) );
        }
    }


    QWidget* contents = new QWidget();
    contents->setLayout(new QVBoxLayout());
    for (int i =0 ; i < 20 ; i++) {
        contents->layout()->addWidget( new QLabel( QString("aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnnooopppqqqrrrssstttuuuvvvwwwxxxyyyzzz") + QString::number(i) ) );
    }
    sclArea->setWidget(contents);



    parent->show();

    app.exec();
}


