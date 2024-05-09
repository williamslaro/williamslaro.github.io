---
layout: archive
title: "Research"
permalink: /publications/
author_profile: true
---

Here is a copy of my [CV]("http://williamslaro.github.io/files/Williams CV.pdf").

{% if site.author.googlescholar %}
  <div class="wordwrap">You can also find my articles on <a href="{{site.author.googlescholar}}">my Google Scholar profile</a>.</div>
{% endif %}

{% include base_path %}

{% for post in site.publications reversed %}
  {% include archive-single.html %}
{% endfor %}
