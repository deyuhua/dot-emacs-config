;; ************************************************************
;; 	Must Load ox-publish package
;; ************************************************************
(use-package org
  :ensure org-plus-contrib
  :defer t)

(require 'ox-md)
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

(advice-add #'org-export-to-file :around (apply-partially #'pkg-with-theme 'doom-dracula-alt))
(advice-add #'org-export-to-buffer :around (apply-partially #'pkg-with-theme 'doom-dracula-alt))

;; ************************************************************
;; 	Force publish all
;; ************************************************************
(use-package htmlize)
(defun pkg-org-publish ()
  (interactive)
  (progn
    (org-reload)
    (org-publish-remove-all-timestamps)
    (org-publish-all t)
    (load-theme 'doom-dracula-alt)    
    (set-face-background 'vertical-border (face-background 'default))
    (set-face-foreground 'vertical-border "grey")
    ))


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
	  "#+OPTIONS: toc:nil\n"
	  "#+KEYWORDS:技术博客,技术思考,机器学习,深度学习,IoT,边缘计算,Kubernets,容器技术\n"
	  "#+DESCRIPTION:前沿技术博客,记录技术生活点滴,Dont't Panic\n\n"
	  "* Articals\n"
	  (replace-regexp-in-string "\*" " " (org-list-to-subtree list))
	  "\n\n"
	  (pkg-file-contents (expand-file-name "~/.emacs.d/core/aboutme.org"))
	  ))

(defun pkg-org-publish-org-sitemap-format (entry style project)
  "Custom sitemap entry formatting: add date"
  (cond ((not (directory-name-p entry))
         (format "- [[file:%s][ %s]]"
                 entry
                 (org-publish-find-title entry project)))
        ((eq style 'tree)
         ;; Return only last subdir.
         (concat "+ "
	 	 (capitalize (file-name-nondirectory (directory-file-name entry)))
	 	 "/"))
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
	 :html-head-include-scripts nil	 
         :html-head "

<meta name=\"viewport\" content=\"width=device-width,initial-scale=1,maximum-scale=1,user-scalable=no\"/>
<meta name=\"baidu-site-verification\" content=\"VsK7KMhTM1\" />
<link rel=\"stylesheet\" href=\"/style/solarized-light.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"/style/global.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"https://use.fontawesome.com/releases/v5.6.3/css/all.css\" integrity=\"sha384-UHRtZLI+pbxtHCWp1t77Bi1L4ZtiqrqD80Kn4Z8NTSRyMA2Fd33n5dQ8lWUE00s/\" crossorigin=\"anonymous\">
<link href=\"/images/favicon.png\" rel=\"icon\">

<link rel=\"apple-touch-icon\" sizes=\"57x57\" href=\"/apple-icon-57x57.png\">
<link rel=\"apple-touch-icon\" sizes=\"60x60\" href=\"/apple-icon-60x60.png\">
<link rel=\"apple-touch-icon\" sizes=\"72x72\" href=\"/apple-icon-72x72.png\">
<link rel=\"apple-touch-icon\" sizes=\"76x76\" href=\"/apple-icon-76x76.png\">
<link rel=\"apple-touch-icon\" sizes=\"114x114\" href=\"/apple-icon-114x114.png\">
<link rel=\"apple-touch-icon\" sizes=\"120x120\" href=\"/apple-icon-120x120.png\">
<link rel=\"apple-touch-icon\" sizes=\"144x144\" href=\"/apple-icon-144x144.png\">
<link rel=\"apple-touch-icon\" sizes=\"152x152\" href=\"/apple-icon-152x152.png\">
<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"/apple-icon-180x180.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"192x192\"  href=\"/android-icon-192x192.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"/favicon-32x32.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"96x96\" href=\"/favicon-96x96.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"/favicon-16x16.png\">
<link rel=\"manifest\" href=\"/manifest.json\">
<meta name=\"msapplication-TileColor\" content=\"#ffffff\">
<meta name=\"msapplication-TileImage\" content=\"/ms-icon-144x144.png\">
<meta name=\"theme-color\" content=\"#ffffff\">

<script
     src=\"https://code.jquery.com/jquery-3.3.1.min.js\"
     integrity=\"sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8=\"
     crossorigin=\"anonymous\">
</script>

<script>
(function(){
    if (location.hostname !== 'huadeyu.tech') {
        return;
    }
    var bp = document.createElement('script');
    var curProtocol = window.location.protocol.split(':')[0];
    if (curProtocol === 'https') {
        bp.src = 'https://zz.bdstatic.com/linksubmit/push.js';
    }
    else {
        bp.src = 'http://push.zhanzhang.baidu.com/push.js';
    }
    var s = document.getElementsByTagName(\"script\")[0];
    s.parentNode.insertBefore(bp, s);
})();
</script>

<script>
var _hmt = _hmt || [];
(function() {
  if (location.hostname !== \"huadeyu.tech\") {
    return;
  }
  var hm = document.createElement(\"script\");
  hm.src = \"https://hm.baidu.com/hm.js?0f9fde052ac9166486f2761c80b2bc93\";
  var s = document.getElementsByTagName(\"script\")[0]; 
  s.parentNode.insertBefore(hm, s);
})();
</script>
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
  <div id=\"editor\">Created By OrgMode; <span id=\"love\" style=\"color: #ff79c6; font-size: 30px;\">♥</span><a href=\"https://zh.wikipedia.org/wiki/Emacs\">#EMACS</a></div>
  <div>Edited By 华德禹 (Deyu Hua) </div>
</div>

<div id=\"icons\">
   <div id=\"nav\">
   <div id=\"home\">
     <a href=\"/index.html\">Home</a>
   </div>
   <div id=\"github\">
     <a href=\"https://github.com/deyuhua\" target=\"_blank\">Github</a>
   </div>
   <div id=\"mail\">
    <a href=\"mailto:deyuhua@gmail.com\">Email</a>
   </div>
   </div>
   <div id=\"avstar\">
     <a href=\"index.html\"><img src=\"../images/v.jpeg\"></a>
   </div>
</div>

<div class=\"back-to-top\">
  <a href=\"#top\"><i class=\"far fa-caret-square-up\"></i></a>
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

   jQuery('.back-to-top').click(function() {
        jQuery('body,html').animate({scrollTop:0},500);
        return false;
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
  (function() {
      if (location.hostname !== 'huadeyu.tech') {
      	//return;
      }
      var footer = document.querySelector('#footer');
      var editor = document.querySelector('#editor');
      var script = document.createElement('script');
      script.src='https://utteranc.es/client.js';
      script.setAttribute('repo', 'deyuhua/deyuhua.github.io');
      script.setAttribute('issue-term', 'pathname');
      footer.insertBefore(script, editor);
  })();  
</script>
")

(provide 'pkg-publish)
