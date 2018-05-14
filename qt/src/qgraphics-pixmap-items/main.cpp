#include <QtGui>

int main (int argc, char *argv[])
{
    QApplication app(argc, argv);

    QGraphicsScene scene;
    QGraphicsView view(&scene);
    QGraphicsPixmapItem pm(QPixmap("/home/keijuterazono/projects/til/qt/src/qgraphics-pixmap-items/coco2.png"));


    QGraphicsDropShadowEffect* effect = new QGraphicsDropShadowEffect();
    effect->setColor( QColor(0,0,0) );
    effect->setBlurRadius(20);
    pm.setGraphicsEffect(effect);

    scene.addItem(&pm);

    view.setStyleSheet("background:transparent; border:none;");
//    view.setAttribute(Qt::WA_TranslucentBackground);
//    view.setWindowFlags(Qt::FramelessWindowHint);
    view.setBackgroundBrush(QBrush(Qt::blue));
    view.resize(500,500);
    view.show();

    app.exec();
}

