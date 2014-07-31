(define (disqus site post)
  (when (post "comments")
    (format 
   "<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
var disqus_shortname = '~a';
var disqus_title = '~a';
var disqus_url = '~a';
/* * * DON'T EDIT BELOW THIS LINE * * */
(function() {
  var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
  dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
  (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
})();
</script>
<noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
<a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>" 
   (or (site "disqus-shortname") (site "url"))
   (post "title")
   (~a #;(site "url") "http://blog.jverkamp.com" "/" (post "permalink") "/"))))

(register-plugin 'disqus disqus)
