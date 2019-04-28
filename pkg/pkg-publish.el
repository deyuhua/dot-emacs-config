;; ************************************************************
;; 	Must Load ox-publish package
;; ************************************************************
(require 'ox)
(require 'ox-publish)


;; ************************************************************
;; 	Setup export theme
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (use-package dracula-theme)

(defun pkg-with-theme (theme fn &rest args)
  (let ((current-themes custom-enabled-themes))
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (let ((result (apply fn args)))
      (mapcar #'disable-theme custom-enabled-themes)
      (mapcar (lambda (theme) (load-theme theme t)) current-themes)
      result)))

(advice-add #'org-export-to-file :around (apply-partially #'pkg-with-theme 'dracula))
(advice-add #'org-export-to-buffer :around (apply-partially #'pkg-with-theme 'dracula))

;; ************************************************************
;; 	Force publish all
;; ************************************************************
(use-package htmlize)
(defun pkg-org-publish ()
  (interactive)
  (progn
    (org-publish-remove-all-timestamps)
    (org-publish-all t)))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 	Load aboutme org-file
;; ************************************************************
(defun pkg-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))


;; ************************************************************
;; 	Change default sitemap index
;; ************************************************************
;; sitemap function
(defun pkg-org-publish-org-sitemap (title list)
  "Sitemap generation function."
  (concat (format "#+TITLE: %s\n" title)
	  "#+OPTIONS: toc:nil\n\n"
	  "* Articals\n"
	  (replace-regexp-in-string "\* ?" "" (org-list-to-subtree list))
	  "\n\n"
	  (pkg-file-contents (expand-file-name "~/.emacs.d/pkg/aboutme.org"))
	  ))

(defun pkg-org-publish-org-sitemap-format (entry style project)
  "Custom sitemap entry formatting: add date"
  (cond ((not (directory-name-p entry))
         (format "- [[file:%s][ %s]]"
                 entry
                 (org-publish-find-title entry project)))
        ((eq style 'tree) "")
         ;; Return only last subdir.
         ;; (concat "- "
	 ;; 	 (capitalize (file-name-nondirectory (directory-file-name entry)))
	 ;; 	 "/"))
        (t entry)))

;; ************************************************************
;; 	Notebook related settings
;; ************************************************************
;; insert src block easily
(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "/Users/deyuhua/Documents/org/notebooks/"
         :base-extension "org"
         :publishing-directory "/Users/deyuhua/Workspace/Documents/网站生成/notebooks/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc t
         :html-head "<link rel=\"stylesheet\"
                       href=\"/style/solarized-dark.css\" type=\"text/css\"/>
                     <script
                       src=\"https://code.jquery.com/jquery-3.3.1.min.js\"
                       integrity=\"sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8=\"
                       crossorigin=\"anonymous\"></script>
                     <link href=\"/images/favicon.ico\" rel=\"icon\">
"
	 ;; :html-preamble t
         :recursive t
         :with-email "deyuhua@gmail.com"
         :with-title t
         :html-html5-fancy t
         :auto-sitemap t
	 :sitemap-function pkg-org-publish-org-sitemap
	 :sitemap-format-entry pkg-org-publish-org-sitemap-format
         :sitemap-filename "index.org"
         :sitemap-title "Don't Panic!"
         )

        ("images"
	 :recursive t
         :base-directory "/Users/deyuhua/Documents/org/notebooks/images/"
         :base-extension "jpg\\|gif\\|png\\|jpeg\\|ico"
         :publishing-directory "/Users/deyuhua/Workspace/Documents/网站生成/notebooks/images/"
         :publishing-function org-publish-attachment)

        ("style"
         :base-directory "/Users/deyuhua/Documents/org/notebooks/style/"
         :base-extension "css\\|el\\|js"
         :publishing-directory "/Users/deyuhua/Workspace/Documents/网站生成/notebooks/style/"
         :publishing-function org-publish-attachment)

        ("fonts"
         :base-directory "/Users/deyuhua/Documents/org/notebooks/fonts/"
         :base-extension "eot\\|woff2\\|woff\\|ttf\\|svg"
         :publishing-directory "/Users/deyuhua/Workspace/Documents/网站生成/notebooks/fonts/"
         :publishing-function org-publish-attachment)	

        ("website" :components ("orgfiles" "images" "style" "fonts"))))

;; static page setup
(setq org-html-preamble t)
(setq org-html-postamble "

<div id=\"footer\">
  <div>Created By OrgMode; <span id=\"love\" style=\"color: #ff79c6; font-size: 30px;\">♥</span><a href=\"https://zh.wikipedia.org/wiki/Emacs\">#EMACS</a></div>
  <div>Edited By 华德禹 (Deyu Hua) </div>
</div>

<div id=\"icons\">
   <div id=\"home\">
     <a href=\"/index.html\">Home</a>
   </div>
   <div id=\"github\">
     <a href=\"https://github.com/deyuhua\">Github</a>
   </div>
  <div id=\"mail\">
    <a href=\"mailto:deyuhua@gmail.com\">Email</a>
  </div>
</div>

<div id=\"back-to-top\">
  <a href=\"#top\">Back2Top</a>
</div>

<script type=\"text/javascript\">
    var offset = 220;
    var duration = 500;
    jQuery(window).scroll(function() {
        if (jQuery(this).scrollTop() > offset) {
            jQuery('.back-to-top').fadeIn(duration);
        } else {
            jQuery('.back-to-top').fadeOut(duration);
        }
    });
   let timer = true;
   setInterval(function() {
	if (timer)
          $(\"#love\").animate({fontSize: 18})
	else
	  $(\"#love\").animate({fontSize: 24})
	timer = !timer 
   }, 300);
</script>
<script>
var _hmt = _hmt || [];
(function() {
  var hm = document.createElement(\"script\");
  hm.src = \"https://hm.baidu.com/hm.js?d3d296d75f0ca0737ee4d3fd4d3d4af2\";
  var s = document.getElementsByTagName(\"script\")[0]; 
  s.parentNode.insertBefore(hm, s);
})();
</script>

<script>
  (function() {
      if (location.pathname === '/') {
      	return;
      }
      var main = document.querySelector('#postamble');
      var script = document.createElement('script');
      script.src='https://utteranc.es/client.js'
      script.setAttribute('repo', 'deyuhua/deyuhua.github.io');
      script.setAttribute('issue-term', 'pathname');
      main.appendChild(script);
  })();  
</script>
")

(provide 'pkg-publish)
