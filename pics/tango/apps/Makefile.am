
size = scalable
context = apps

iconsdir = $(themedir)/$(size)/$(context)

icons_DATA =						\
	accessories-calculator.svg			\
	accessories-character-map.svg			\
	accessories-text-editor.svg			\
	help-browser.svg				\
	internet-group-chat.svg				\
	internet-mail.svg				\
	internet-news-reader.svg			\
	internet-web-browser.svg			\
	office-calendar.svg				\
	preferences-desktop-accessibility.svg		\
	preferences-desktop-assistive-technology.svg	\
	preferences-desktop-font.svg			\
	preferences-desktop-keyboard-shortcuts.svg	\
	preferences-desktop-locale.svg	\
	preferences-desktop-multimedia.svg	\
	preferences-desktop-remote-desktop.svg		\
	preferences-desktop-screensaver.svg		\
	preferences-desktop-theme.svg			\
	preferences-desktop-wallpaper.svg		\
	preferences-system-network-proxy.svg		\
	preferences-system-session.svg			\
	preferences-system-windows.svg			\
	system-file-manager.svg				\
	system-installer.svg				\
	system-software-update.svg			\
	system-users.svg				\
	utilities-system-monitor.svg			\
	utilities-terminal.svg

EXTRA_DIST =						\
	$(icons_DATA)

install-data-local: install-iconsDATA
	(cd $(DESTDIR)$(themedir)/$(size) && $(ICONMAP) -c $(context))
if ENABLE_LARGE_BITMAPS
	for i in 48 64 72 96 128; do \
		pngdir="$(DESTDIR)$(themedir)/$${i}x$${i}/$(context)"; \
		$(mkinstalldirs) $$pngdir; \
		for icon in $(icons_DATA); do \
			$(top_builddir)/svg2png.sh $$i $$pngdir $(srcdir)/$$icon; \
		done; \
		(cd $(DESTDIR)$(themedir)/$${i}x$${i} && $(ICONMAP) -c $(context)); \
	done
endif

uninstall-local:
	for i in 48 64 72 96 128; do \
		pngdir="$(DESTDIR)$(themedir)/$${i}x$${i}/$(context)"; \
		for icon in $(icons_DATA); do \
			iname=`echo $$icon|sed -e "s/svg/png/g"`; \
			rm -f $$pngdir/$$iname; \
		done; \
	done

