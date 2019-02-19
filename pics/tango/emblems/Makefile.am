
size = scalable
context = emblems

iconsdir = $(themedir)/$(size)/$(context)

icons_DATA =				\
	emblem-favorite.svg		\
	emblem-important.svg		\
	emblem-photos.svg		\
	emblem-readonly.svg		\
	emblem-symbolic-link.svg	\
	emblem-system.svg		\
	emblem-unreadable.svg

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

