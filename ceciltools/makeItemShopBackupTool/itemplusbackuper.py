#! python3
import winreg
import re
#import struct
#import win32com.client


APP_PATH = r'SOFTWARE\DreamFactory\ItemShop'
BOX_KEYS = ['ItemBoxButton',
           'ItemTexSlider',
           'ItemMoveXSlider',
           'ItemMoveYSlider',
           'ItemMoveZSlider',
           'ItemRotXSlider',
           'ItemRotYSlider',
           'ItemRotZSlider',
           'ItemSizeXSlider',
           'ItemSizeYSlider',
           'ItemSizeZSlider',
           'ItemBoneSlider',
           'ItemColoirRSlider',
           'ItemColoirGSlider',
           'ItemColoirBSlider',
           'ItemTexSlider [(]1[)]',
           'ItemColoirRSlider [(]1[)]',
           'ItemColoirGSlider [(]1[)]',
           'ItemColoirBSlider [(]1[)]',
           'ItemColoirASlider [(]1[)]',
           'ItemShapeSlider',
           'ItemShapeSlider [(]1[)]',
           'ItemShapeSlider [(]2[)]',
           'ItemShapeSlider [(]3[)]',
           ]

def loadCurrentItemBoxes():
    # rrr = RowRegReader()

    with winreg.OpenKeyEx(winreg.HKEY_CURRENT_USER, APP_PATH) as itemShopKey:
        n_subkeys, n_subvalues, timestamp = winreg.QueryInfoKey(itemShopKey)

        itemboxes = [{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}]
        for i in range(n_subvalues):
            name, value, regtyp = winreg.EnumValue(itemShopKey,i)


            for boxKey in BOX_KEYS:
                regstr = (r'^%s([0-9]+)_h[0-9]+$' % boxKey)
                m = re.match( regstr, name)
                if m:
                    # memo:
                    #   Cecil ItemPlusは REG_DWORD(32bit)に double(64bit) を
                    #   詰め込んでしまってるので、winreg だと上位32bitしか
                    #   読み出すことが出来ない。詰んだ。
                    #   (以下、Ex付かないせいと勘違いしてためしに読んでみたコード）
                    # v, rt = winreg.QueryValueEx(itemShopKey, name)
                    # print( '%s: %s' % (name, v))
                    #
                    # memo:
                    #   ひょっとしたらwsh経由ならいけるかなーとおもったけど
                    #   だめだった
                    # rawv = rrr.read(name)
                    # print( "   %x vs %x" % (value, rawv))
                    itemboxes[int(m.group(1))][boxKey] = name, value, regtyp
                    continue

    return itemboxes

class RowRegReader():
    def __init__(self):
        self.wshShell = win32com.client.Dispatch("WScript.Shell")

    def read(self, name):
        return self.wshShell.RegRead(r'HKCU\%s\%s' % (APP_PATH, name))



if __name__ == '__main__':

    itemboxes = loadCurrentItemBoxes()

    itemnum = 0
    for itembox in itemboxes:
        print( '- item %d' % itemnum )
        itemnum += 1

        if itembox['ItemBoxButton'][1] == 0xffffffff:
            print( "   - (none)" )
            continue

        for boxkey in BOX_KEYS:
            try:
                v = itembox[boxkey]
                print( "   - {:<38s} -> {:X}".format(v[0], v[1]) )
            except KeyError:
                print( "   - {:<38s} -> (none)".format(boxkey) )
