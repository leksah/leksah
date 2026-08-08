
size = scalable
context = actions

iconsdir = $(themedir)/$(size)/$(context)

icons_DATA =				\
	address-book-new.svg		\
	appointment-new.svg		\
	bookmark-new.svg		\
	contact-new.svg			\
	document-new.svg		\
	document-open.svg		\
	document-print.svg		\
	document-print-preview.svg	\
	document-properties.svg		\
	document-save.svg		\
	document-save-as.svg		\
	edit-clear.svg			\
	edit-copy.svg			\
	edit-cut.svg			\
	edit-delete.svg	\
	edit-find.svg			\
	edit-find-replace.svg			\
	edit-paste.svg			\
	edit-redo.svg			\
	edit-select-all.svg	\
	edit-undo.svg			\
	folder-new.svg			\
	format-indent-less.svg		\
	format-indent-more.svg		\
	format-justify-center.svg	\
	format-justify-fill.svg		\
	format-justify-left.svg		\
	format-justify-right.svg	\
	format-text-bold.svg		\
	format-text-italic.svg		\
	format-text-strikethrough.svg	\
	format-text-underline.svg	\
	go-bottom.svg			\
	go-down.svg			\
	go-first.svg			\
	go-home.svg			\
	go-jump.svg			\
	go-last.svg			\
	go-next.svg			\
	go-previous.svg			\
	go-top.svg			\
	go-up.svg			\
	list-add.svg	\
	list-remove.svg	\
	mail-forward.svg	\
	mail-message-new.svg		\
	mail-mark-junk.svg	\
	mail-reply-all.svg	\
	mail-reply-sender.svg	\
	mail-send-receive.svg	\
	media-eject.svg	\
	media-playback-pause.svg	\
	media-playback-start.svg	\
	media-playback-stop.svg		\
	media-record.svg		\
	media-seek-backward.svg		\
	media-seek-forward.svg		\
	media-skip-backward.svg		\
	media-skip-forward.svg		\
	process-stop.svg		\
	system-lock-screen.svg		\
	system-log-out.svg		\
	system-search.svg		\
	system-shutdown.svg		\
	tab-new.svg			\
	view-fullscreen.svg		\
	view-refresh.svg		\
	window-new.svg	

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
