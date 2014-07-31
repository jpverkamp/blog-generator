/* Init highlight.js once everything is loaded */
hljs.initHighlightingOnLoad();

/* Custom init for JS Flickr Gallery to display images inline with Bootstrap */
$(function() {
  $('div.flickr-gallery').jsFlickrGallery({
    'structure': {
      'ulClass': 'list-inline thumbnails',
	  'aClass': 'jsfg-link'
    },
	'modal': false
  });
});

/* Initialize lightboxes on all the images */
$(document).delegate('*[data-toggle="lightbox"]', 'click', function(event) {
    event.preventDefault();
    $(this).ekkoLightbox();
}); 

$(document).delegate('a.jsfg-link', 'click', function(event) {
    event.preventDefault();
    $(this).ekkoLightbox();
}); 