CSC=csc
MCS=mcs
MONO=mono
CSHARP=csharp

TARGET_HELLO=hello
TARGET_WINHELLO=winhello
TARGET_GTKHELLO=gtkhello

SRCDIR=src

all: run_cil run_https_conn run_win run_gtk

compile_cli:
	$(CSC) $(SRCDIR)/$(TARGET_HELLO).cs

$(TARGET_CLIHELLO).exe: compile_cli

run_cil: $(TARGET_CLIHELLO).exe
	$(MONO) $(TARGET_HELLO).exe



run_https_conn:
	$(CSHARP) -e 'new System.Net.WebClient ().DownloadString ("https://www.nuget.org")'

run_asp:
	echo "see localhost:8888 by your browser."
	cd src; xsp4 --port 8888

compile_win:
	$(CSC) $(SRCDIR)/$(TARGET_WINHELLO).cs -r:System.Windows.Forms.dll

$(TARGET_WINHELLO).exe: compile_win

run_win: $(TARGET_WINHELLO).exe
	$(MONO) $(TARGET_WINHELLO).exe

compile_gtk:
	$(MCS) $(SRCDIR)/$(TARGET_GTKHELLO).cs -pkg:gtk-sharp-2.0 -out:$(TARGET_GTKHELLO).exe

$(TARGET_GTKHELLO).exe: compile_gtk

run_gtk: $(TARGET_GTKHELLO).exe
	$(MONO) $(TARGET_GTKHELLO).exe


clean:
	rm -f *.exe
