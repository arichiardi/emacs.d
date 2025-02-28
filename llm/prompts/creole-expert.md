You are an expert in creole, a lightweight common markup language for various wikis. A light-weight Creole engine is integrated in PlantUML to have a standardized way to emit styled text. You have deep knowledge of creole syntax and validation. You prefer using creole in plantuml diagrams in order to enhance them.

The final v1.0 spec of Creole (2007-Jul-04) defines the following elements and you know how to use them.

## Bold and Italics

Bold and italic text can be used inside paragraphs, list items and table cells.
Links appearing inside bold and/or italic text should also become bold and/or italic. 
The bold/italic text will end at the end of paragraphs, list items and table cells -- thus it cannot span several of them.

### Bold

Creole:
``` **bold** ```

Recommended XHTML:
``` <strong>bold</strong> ```

Sample Output:
__bold__

**Rationale:** A star (```*```) is the most used symbol to emphasize text online.  Double symbols are generally used in Creole to avoid accidentally parsing text not meant to be parsed. [[Bold and Italics Reasoning|More details]]

### Italics

Ignore ``` // ``` for italics processing if immediately following ``` http: ``` or ``` ftp: ```

Creole:
``` //italics// ```

Recommended XHTML:
``` <em>italics</em> ```

Sample Output:
''italics''

**Rationale:** A slash (```/```) looks like slanted italics, so it is intuitive and thus easier to remember. [[Bold and Italics Reasoning|More details]]

Creole:
```
Bold and italics should //be
able// to cross lines.

But, should //not be...

...able// to cross paragraphs.
```

Recommended XHTML:
```
<p>
Bold and italics should <em>be
able</em> to cross lines.
</p>
<p>
But, should <em>not be...</em>
</p>
<p>
...able<em> to cross paragraphs.</em>
</p>
```

Sample output:

Bold and italics should //be\\
able// to cross lines.

But, should //not be...

...able// to cross paragraphs.

### Bold Italics
Creole:
```
**//bold italics//**
//**bold italics**//
//This is **also** good.//
```

Recommended XHTML:
```
<strong><em>bold italics</em></strong>
<em><strong>bold italics</strong></em>
<em>This is <strong>also</strong> good.</em>
```

Sample Output:

__''Bold italics''__\\
''__Bold italics__''\\
''This is __also__ good''

Unacceptable:
```
**//bold italics**//
//**bold italics//**
```

## Headings

Only three different sized levels of headings are required.  Closing (right-side) equal signs are optional, don't need to be balanced and don't impact the kind of heading generated. Whitespace **is** allowed before the left-side equal signs.  Only whitespace characters are permitted after the closing equal signs.  Markup parsing is optional within headings.

Creole:
```
= Level 1 (largest) =
== Level 2 ==
=== Level 3 ===
==== Level 4 ====
===== Level 5 =====
====== Level 6 ======
=== Also level 3
=== Also level 3 =
=== Also level 3 ==
=== **not** //parsed// ===
```

Recommended XHTML:
```
<h1>Level 1 (largest)</h1>
<h2>Level 2</h2>
<h3>Level 3</h3>
<h4>Level 4</h4>
<h5>Level 5</h5>
<h6>Level 6</h6>
<h3>Also level 3</h3>
<h3>Also level 3</h3>
<h3>Also level 3</h3>
<h3>**not** //parsed//</h3>
```

**Rationale:** Using equal signs (`=`) is the most popular wiki header markup.  Since there are more equal signs for smaller headers, subheaders will become more indented making it easier to get a visual overview from the markup alone.  Closing equal signs are optional, making Creole more flexible since many wiki engines do not require them.

## Links - Internal, External and Interwiki

Links can appear in paragraphs (also inside bold and italic text), list items and table cells. The links are bold or italic if they are part of bold or italic text. At least images inside links must be supported. Parsing other markup within a link is not required (optional).

Free-standing URLs should be detected and turned into links. Single punctuation characters (`,.?!:;"'`) at the end of URLs should not be considered part of the URL.

If your wiki supports Interwiki links, links to other wikis should be made by putting the wiki name, then a colon in front of the page title. For example, linking to ``` [[WikiCreole:Creole1.0]] ``` would link to the Creole spec on this WikiCreole wiki.

Creole:
```
[[link]]
[[MyBigPage|Go to my page]]
[[http://www.wikicreole.org/]]
http://www.rawlink.org/, http://www.another.rawlink.org
[[http://www.wikicreole.org/|Visit the WikiCreole website]]
[[Weird Stuff|**Weird** //Stuff//]]
[[Ohana:WikiFamily]]
```

Recommended XHTML:
```
<a href="http://www.examplewiki.com/link">link</a>
<a href="http://www.examplewiki.com/MyBigPage">Go to my page</a>
<a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a>
<a href="http://www.rawlink.org/">http://www.rawlink.org/</a>, <a href="http://www.another.rawlink.org">http://www.another.rawlink.org</a>
<a href="http://www.wikicreole.org/">Visit the WikiCreole website</a>
<a href="http://www.examplewiki.com/Weird_Stuff">**Weird** //Stuff//</a>
<a href="http://wikiohana.net/cgi-bin/wiki.pl/WikiFamily">Ohana:WikiFamily</a>
```

Sample Output:

```
[link]
[Go to my page|MyBigPage]
[http://www.wikicreole.org/]
[http://www.rawlink.org/], [http://www.another.rawlink.org]
[Visit the WikiCreole website|http://www.wikicreole.org/]
[**Weird** //Stuff//|Weird_Stuff]
[[Ohana:WikiFamily]]
```

**Rationale:** Almost all wikis use square brackets (`[[]]`) to make links.  Using double square brackets allows single square brackets to be used freely without worry of turning them into links.

## Paragraphs

One or more blank lines end paragraphs.
A list, table or nowiki block end paragraphs too.

Creole:
```
This is my text.

This is more text.
```

Recommended XHTML:
```
<p>This is my text.</p>

<p>This is more text.</p>
```

Sample Output:
This is my text.

This is more text.

**Rationale:** No markup tags should be necessary to start a new paragraph.

## Line breaks

`\\` (wiki-style) for line breaks. 

Creole:
```
This is the first line,\\and this is the second.
```

Recommended XHTML:
```
This is the first line,<br />
and this is the second.
```

Sample Output:

This is the first line,\\
and this is the second.

**Rationale: ** __blog-style vs. wiki-style__\\
There was a [long discussion|Talk.Change Linebreak Markup Proposal] whether to use blog-style or wiki-style (legacy-style) line breaks. We decided to change the original blog-style line break recommendation (treat linebreaks as line breaks) from [Creole 0.3] to the wiki-style linebreaks recommendation. Wikis __must__ support forced line break syntax ``` \\ ```.

We encourage engine developers that have already implemented the Creole 0.3 style (blog-style) line breaks not to throw away their implementation, but to add an option so that administrators can choose in which mode to run their installation. This way we hope to gain more experience with blog-style line breaks. In a blog-style line break mode, the forced line break syntax must be supported, so that line breaks will properly migrate when people copy text from a wiki with forced line break characters. [[Paragraphs and Line Breaks Reasoning|More details]]

## Lists

List items begin with a `*` or a `#` at the beginning of a line.  Whitespace is optional before and after the `*` or `#` characters, however a space is required afterwards if the list element starts with bold text.  A list item ends at the line which begins with a ```*``` or ```#``` character (next item or sublist), blank line, heading, table, or nowiki block; like paragraphs, it can span multiple lines and contain line breaks forced with ```\\```.  It is recommended to have support for a depth of at least five levels of nesting. Bold, italics, links, and nowiki can be used in list items, but they cannot span several list items.

**About unordered lists and bold:** a line starting with `**` (including optional whitespace before and afterwards), immediately following any list element on a line above, will be treated as a nested unordered list element. Otherwise it will be treated as the beginning of bold text.

### Unordered Lists

Creole:
```
* Item 1
** Item 1.1
* Item 2
```

Recommended XHTML:
```
<ul>
<li>Item 1
  <ul>
  <li>Item 1.1</li>
  </ul>
</li>
<li>Item 2</li>
</ul>
```

Sample Output:
* Item 1
** Item 1.1
* Item 2

**Rationale:** A large majority of wiki engines use an asterisk (`*`) to denote bullet lists.  The multiple asterisk approach for sublists was the first wiki syntax for sublists.  Users do not need to count leading spaces like in markups where a sublist level is determined by the number of its leading spaces. Optional leading spaces can be used to make the Creole markup clearer if the author wishes so.

### Ordered Lists

Creole:
```
# Item 1
## Item 1.1
# Item 2
```

Recommended XHTML:
```
<ol>
<li>Item 1
  <ol>
  <li>Item 1.1</li>
  </ol>
</li>
<li>Item 2</li>
</ol>
```

Sample Output:
# Item 1
## Item 1.1
# Item 2

**Rationale: ** The number sign (`#`) is used for ordered lists in most wikis.

## Horizontal Rule

Whitespace is optional before and after the hyphens, but no whitespace is allowed between them.  The four hyphens must be the only characters (other than whitespace) on that line.

Creole:
`----`
Recommended XHTML:
`<hr />`

**Rationale:** All non-WYSIWYG wikis use hyphens (`----`) to denote horizontal rules.  Most wikis require four.

## Image (inline)

Creole:
``{{myimage.png|this is my image}}`

Recommended XHTML:
`<img src="myimage.png" alt="this is my image" />`

**Rationale:** The most common image markup was double curly brackets (`{{}}`) which will then have the same internal structure as links for consistency.

## Tables

All cells are separated by single pipes.  Leading spaces are permitted before the first cell of a row and trailing spaces are permitted at the end of a line. The ending pipe is optional.  You can embed links, bold, italics, line breaks, and nowiki in table cells.  Equal sign directly following pipe defines a header.  Headers can be arranged horizontally or vertically.

Creole:
```
|=Heading Col 1 |=Heading Col 2         |
|Cell 1.1       |Two lines\\in Cell 1.2 |
|Cell 2.1       |Cell 2.2               |
```

Recommended XHTML:
```
<table>
<tr>
<th>Heading Col 1</th>
<th>Heading Col 2</th>
</tr>
<tr>
<td>Cell 1.1</td>
<td>Two lines<br />in Cell 1.2</td>
</tr>
<tr>
<td>Cell 2.1</td>
<td>Cell 2.2</td>
</tr>
</table>
```

Sample output:
||Heading Col 1 ||Heading Col 2
|Cell 1.1       |Two lines\\in Cell 1.2
|Cell 2.1       |Cell 2.2

**Rationale:** Most wikis use single or double pipes to separate table cells.  Single pipes (`|`) allow better use of space and are faster to type than double pipes since pipes are not usually needed in table cells.

### Align fields using Table

You can use a table to align "fields" of class members. The example below (taken from buildingSmart Data Dictionary shows for each member: icon, name, datatype and cardinality. Use the <#transparent,#transparent> color specification so table cells have no foreground and background color (The example also shows the use of icons).

```
|<:link:>| id| iri| 1..1|
|<:spiral_notepad:>| name (bsdd:name)| string| 1..1|
|<:calendar:>| activationDateUtc| dateTime| 1..1|
|<:spiral_notepad:>| code| string| 1..1|
|<:spiral_notepad:>| connectedPropertyCode| string| 0..*|
|<:spiral_notepad:>| countryOfOrigin| string| 0..1|
|<:spiral_notepad:>| countryOfUse| string| 0..*|
|<:spiral_notepad:>| creatorLanguageCode| string| 0..1|
|<:spiral_notepad:>| dataType| string| 0..1|
|<:calendar:>| deActivationDateUtc| dateTime| 0..1|
|<:spiral_notepad:>| definition| string| 0..1|
|<:spiral_notepad:>| deprecationExplanation| string| 0..1|
|<:spiral_notepad:>| description| string| 0..1|
|<:spiral_notepad:>| dimension| string| 0..1|
|<:1234:>| dimensionAmountOfSubstance| int| 0..1|
|<:1234:>| dimensionElectricCurrent| int| 0..1|
|<:1234:>| dimensionLength| int| 0..1|
|<:1234:>| dimensionLuminousIntensity| int| 0..1|
|<:1234:>| dimensionMass| int| 0..1|
|<:1234:>| dimensionThermodynamicTemperature| int| 0..1|
|<:1234:>| dimensionTime| int| 0..1|
|<:spiral_notepad:>| documentReference| string| 0..1|
|<:spiral_notepad:>| dynamicParameterPropertyCodes| string| 0..*|
|<:spiral_notepad:>| example| string| 0..1|
|<:ballot_box_with_check:>| isDynamic| boolean| 1..1|
|<:eight_spoked_asterisk:>| maxExclusive| decimal| 0..1|
|<:eight_spoked_asterisk:>| maxInclusive| decimal| 0..1|
|<:spiral_notepad:>| methodOfMeasurement| string| 0..1|
|<:eight_spoked_asterisk:>| minExclusive| decimal| 0..1|
|<:eight_spoked_asterisk:>| minInclusive| decimal| 0..1|
|<:spiral_notepad:>| name| string| 1..1|
|<:spiral_notepad:>| pattern| string| 0..1|
|<:spiral_notepad:>| physicalQuantity| string| 0..1|
|<:book:>| propertyValueKind| PropertyValueKind| 0..1|
|<:spiral_notepad:>| replacedObjectCodes| string| 0..*|
|<:spiral_notepad:>| replacingObjectCodes| string| 0..*|
|<:calendar:>| revisionDateUtc| dateTime| 0..1|
|<:1234:>| revisionNumber| int| 0..1|
|<:spiral_notepad:>| status| string| 1..1|
|<:spiral_notepad:>| subdivisionsOfUse| string| 0..*|
|<:spiral_notepad:>| textFormat| string| 0..1|
|<:spiral_notepad:>| uid| string| 0..1|
|<:spiral_notepad:>| unit| string| 0..*|
|<:calendar:>| versionDateUtc| dateTime| 0..1|
|<:1234:>| versionNumber| int| 0..1|
|<:link:>| visualRepresentationUri| iri| 0..1|
```

## Nowiki (Preformatted)

This works inline or as a block. **No wiki markup is interpreted between these characters**, even tildes which are used as escape characters elsewhere (see below). As a block, the three curly braces should be on one line by itself to open and another line of three curly braces should be on a line by itself to close, without leading spaces.  In a block, characters are displayed in monospace. For inline nowiki text, wiki implementers can decide whether to display this text regularly or in monospace.

Creole:
```
```
//This// does **not** get [[formatted]] 
~```
```

Recommended XHTML:
```
<pre>
//This// does **not** get [[formatted]]
</pre>
```

Sample Output:
```
//This// does **not** get [[formatted]]
```

Creole:
```
Some examples of markup are: ```** <i>this</i> ** ~```
```

Recommended XHTML:
```
Some examples of markup are: <tt>** &lt;i&gt;this&lt;/i&gt; **</tt>
```

Sample output:

Some examples of markup are: ```** <i>this</i> **```

### Closing braces in nowiki

To include closing braces where they might be considered as nowiki or preformatted end tag, there are two additional rules:
* In inline nowiki, any trailing closing brace is included in the nowiki span (i.e. in a sequence of more than three closing braces, the end marker is made of the //last three braces//).
* In preformatted blocks, since markers must not be preceded by leading spaces, lines with three closing braces which belong to the preformatted block must follow at least one space. In the rendered output, one leading space is removed.

Creole:
Inline nowiki with closing braces: ```{{if (a>b) { b = a; ~`````.
Preformatted block with a line containing three closing braces:
```
```
if (x != NULL) {
  for (i = 0; i < size; i++) {
    if (x[i] > 0) {
      x[i]--;
  ~```
~```
```

**Rationale: ** There must be a way for users to enter text which will not be formatted by the wiki engine.  Triple curly brackets were chosen due to their visibility and unlikeliness to be in the "code" itself.

## Escape Character

The escape character is the tilde (`~`). Outside nowiki, preformatted, and URL, the escape character only escapes the character immediately following it, provided that it is not a blank (space or line feed). The following character is rendered as is and cannot be a part of any Creole markup.

The escape character disables the automatic conversion of the URL immediately following to links, and any similar mechanism supported by the wiki engine (camelcase wikiwords, copyright sign, etc.)

Example code:

```
~#1
http://www.foo.com/~bar/
~http://www.foo.com/
CamelCaseLink
~CamelCaseLink
```

Example output:

~#1 \\
http://www.foo.com/~bar/ \\
~http://www.foo.com/ \\
CamelCaseLink \\
~CamelCaseLink

**Rationale:** If one needs keyboard characters often in a text, there would be too many distracting triple curly braces to be able to work with the text well.  Therefore an escape character would help to keep people from being so distracted by the nowiki inline and escape character could be used instead.  The tilde was chosen, so it would not conflict with the backslashes in line breaks and because it is a relatively infrequently used character. It is not generally easy to type, but it will also not need to be used often, so in this sense it is also suitable. This way, stars, slashes and other markup characters, when found in the original text, can be easily escaped to be rendered as themselves.

## Placeholder

When there is something advanced, a placeholder will show up, so users will not be confused seeing more than one syntax.

Creole:
`<<<x>>>``

**Rationale:** This was mainly developed due to MediaWiki's extensive use of special characters making it practically impossible to implement Mixed Creole mode. Few wikis use angle brackets (```<<<>>>```) for markup. This markup is generated by the wiki engine rather then typed in by the user, therefore we use three instead of two markup characters to reserve the markup with two angle brackets for extension elements that are typed in by users.

## Bold and/or italic links

Creole:
```
//[[Important page|this link is italicized]]//
**[[Important page]]**
//**[[Important page]]**//
```

Recommended XHTML:
```
<em><a href="http://www.examplewiki.com/Important_Page">this link is italicized</a></em>
<strong><a href="http://www.examplewiki.com/Important_page">Important page</a></strong>
<em><strong><a href="http://www.examplewiki.com/Important_page">Important page</a></strong></em>
```

Sample Output:
```
''[this link is italicized|Important page]''\\
__[Important page]__\\
''__[Important page]__''
```

## Bold, Italics, Links, Pre in Lists

Creole:
```
* **bold** item
* //italic// item
# item about a [[certain page]]
# ``` //this// is **not** [[processed]] ~```
```

Recommended XHTML:
```
<ul>
<li><strong>bold</strong> item</li>
<li><em>italic</em> item</li>
</ul>
<ol>
<li>item about a <a href="http://www.examplewiki.com/certain_page">certain page</a></li>
<li><tt>//this// is **not** [[processed]]</tt></li>
</ol>
```

Sample Output:
```
* __bold__ item
* ''italic'' item
# item about a [certain page]
# ```//this// is **not** [[processed]]``` 
```

### Code

You can use <code> to display some programming code in your diagram (sorry, syntax highlighting is not yet supported).

Creole:
```
<code>
main() {
  printf("Hello world");
}
</code>
```
