
size = scalable
context = status

iconsdir = $(themedir)/$(size)/$(context)

icons_DATA =				\
	audio-volume-high.svg		\
	audio-volume-low.svg		\
	audio-volume-medium.svg		\
	audio-volume-muted.svg		\
	battery-caution.svg		\
	dialog-error.svg		\
	dialog-information.svg		\
	dialog-warning.svg		\
	folder-drag-accept.icon		\
	folder-drag-accept.svg		\
	folder-open.svg			\
	folder-visiting.icon		\
	folder-visiting.svg		\
	image-loading.svg		\
	image-missing.svg		\
	mail-attachment.svg		\
	network-error.svg		\
	network-idle.svg		\
	network-offline.svg		\
	network-receive.svg		\
	network-transmit.svg		\
	network-transmit-receive.svg	\
	network-wireless-encrypted.svg	\
	printer-error.svg		\
	software-update-available.svg	\
	software-update-urgent.svg	\
	user-trash-full.svg		\
	weather-clear.svg		\
	weather-clear-night.svg		\
	weather-few-clouds.svg		\
	weather-few-clouds-night.svg	\
	weather-overcast.svg		\
	weather-severe-alert.svg	\
	weather-showers.svg		\
	weather-showers-scattered.svg	\
	weather-snow.svg		\
	weather-storm.svg

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
