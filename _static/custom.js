/* Init highlight.js once everything is loaded */
hljs.initHighlightingOnLoad();

/* Custom init for JS Flickr Gallery to display images inline with Bootstrap */
$(function() {
  $('div.flickr-gallery').jsFlickrGallery({
    'structure': {
      'ulClass': 'list-inline thumbnails',
    },
  });
});
