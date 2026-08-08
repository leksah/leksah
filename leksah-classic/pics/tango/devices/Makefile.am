
size = scalable
context = devices

iconsdir = $(themedir)/$(size)/$(context)

icons_DATA =				\
	audio-card.svg			\
	audio-input-microphone.svg	\
	battery.svg			\
	camera-photo.svg		\
	camera-video.svg		\
	computer.svg			\
	drive-optical.svg		\
	drive-harddisk.svg		\
	drive-removable-media.svg	\
	input-gaming.svg		\
	input-keyboard.svg		\
	input-mouse.svg			\
	media-optical.svg		\
	media-floppy.svg		\
	media-flash.svg			\
	multimedia-player.svg		\
	network-wired.svg		\
	network-wireless.svg		\
	printer.svg			\
	video-display.svg

EXTRA_DIST =				\
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

