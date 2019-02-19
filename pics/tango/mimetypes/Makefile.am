
size = scalable
context = mimetypes

iconsdir = $(themedir)/$(size)/$(context)

icons_DATA =					\
	application-certificate.svg		\
	application-x-executable.svg		\
	audio-x-generic.svg			\
	font-x-generic.svg	\
	image-x-generic.svg			\
	package-x-generic.svg			\
	text-html.svg				\
	text-x-generic.svg			\
	text-x-generic-template.svg		\
	text-x-script.svg			\
	video-x-generic.svg			\
	x-office-address-book.svg		\
	x-office-calendar.svg	\
	x-office-document.svg			\
	x-office-document-template.svg			\
	x-office-drawing.svg		\
	x-office-drawing-template.svg		\
	x-office-presentation.svg	\
	x-office-presentation-template.svg	\
	x-office-spreadsheet.svg	\
	x-office-spreadsheet-template.svg

EXTRA_DIST =					\
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

