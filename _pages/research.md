---
layout: archive
<<<<<<< HEAD
title: "Research"
permalink: /research/
>>>>>>> d920f5ca032122cd46113d4dd01f320152b39603
author_profile: true
---

{% if site.author.googlescholar %}
  <div class="wordwrap">You can also find my articles on <a href="{{site.author.googlescholar}}">my Google Scholar profile</a>.</div>
{% endif %}

{% include base_path %}

{% for post in site.publications reversed %}
  {% include archive-single.html %}
{% endfor %}
