context("htmltab examples work correctly")

test_that("Example 1 works", {

  html_world <- '<table class="wikitable sortable" style="font-size:97%; text-align:right;">
<caption>World historical and predicted populations (in millions)<sup id="cite_ref-Vallin_117-0" class="reference"><a href="#cite_note-Vallin-117"><span>[</span>115<span>]</span></a></sup><sup id="cite_ref-un2004_118-0" class="reference"><a href="#cite_note-un2004-118"><span>[</span>116<span>]</span></a></sup></caption>
  <tr>
    <th>Region</th>
    <th>1500</th>
    <th>1600</th>
    <th>1700</th>
    <th>1750</th>
    <th>1800</th>
    <th>1850</th>
    <th>1900</th>
    <th>1950</th>
    <th>1999</th>
    <th>2008</th>
    <th>2010</th>
    <th>2012</th>
    <th>2050</th>
    <th>2150</th>
    </tr>
    <tr>
    <th>World</th>
    <td>458</td>
    <td>580</td>
    <td>682</td>
    <td>791</td>
    <td>978</td>
    <td>1,262</td>
    <td>1,650</td>
    <td>2,521</td>
    <td>5,978</td>
    <td>6,707</td>
    <td>6,896</td>
    <td>7,052</td>
    <td>8,909</td>
    <td>9,746</td>
    </tr>
    <tr>
    <th>Africa</th>
    <td>86</td>
    <td>114</td>
    <td>106</td>
    <td>106</td>
    <td>107</td>
    <td>111</td>
    <td>133</td>
    <td>221</td>
    <td>767</td>
    <td>973</td>
    <td>1,022</td>
    <td>1,052</td>
    <td>1,766</td>
    <td>2,308</td>
    </tr>
    <tr>
    <th>Asia</th>
    <td>243</td>
    <td>339</td>
    <td>436</td>
    <td>502</td>
    <td>635</td>
    <td>809</td>
    <td>947</td>
    <td>1,402</td>
    <td>3,634</td>
    <td>4,054</td>
    <td>4,164</td>
    <td>4,250</td>
    <td>5,268</td>
    <td>5,561</td>
    </tr>
    <tr>
    <th>Europe</th>
    <td>84</td>
    <td>111</td>
    <td>125</td>
    <td>163</td>
    <td>203</td>
    <td>276</td>
    <td>408</td>
    <td>547</td>
    <td>729</td>
    <td>732</td>
    <td>738</td>
    <td>740</td>
    <td>628</td>
    <td>517</td>
    </tr>
    <tr>
    <th>Latin America<sup id="cite_ref-Americas_119-0" class="reference"><a href="#cite_note-Americas-119"><span>[</span>Note 1<span>]</span></a></sup></th>
    <td>39</td>
    <td>10</td>
    <td>10</td>
    <td>16</td>
    <td>24</td>
    <td>38</td>
    <td>74</td>
    <td>167</td>
    <td>511</td>
    <td>577</td>
    <td>590</td>
    <td>603</td>
    <td>809</td>
    <td>912</td>
    </tr>
    <tr>
    <th>Northern America<sup id="cite_ref-Americas_119-1" class="reference"><a href="#cite_note-Americas-119"><span>[</span>Note 1<span>]</span></a></sup></th>
    <td>3</td>
    <td>3</td>
    <td>2</td>
    <td>2</td>
    <td>7</td>
    <td>26</td>
    <td>82</td>
    <td>172</td>
    <td>307</td>
    <td>337</td>
    <td>345</td>
    <td>351</td>
    <td>392</td>
    <td>398</td>
    </tr>
    <tr>
    <th>Oceania</th>
    <td>3</td>
    <td>3</td>
    <td>3</td>
    <td>2</td>
    <td>2</td>
    <td>2</td>
    <td>6</td>
    <td>13</td>
    <td>30</td>
    <td>34</td>
    <td>37</td>
    <td>38</td>
    <td>46</td>
    <td>51</td>
    </tr>
    </table>
    '

  ex1 <- htmltab(doc = html_world, which = 1)

  expect_that(ex1[1,1], equals("World"))
  expect_that(colnames(ex1)[1], equals("Region"))
})


test_that("Example 2 works", {

  html_world <- '<table class="wikitable sortable" style="font-size:97%; text-align:right;">
  <caption>World historical and predicted populations (in millions)<sup id="cite_ref-Vallin_117-0" class="reference"><a href="#cite_note-Vallin-117"><span>[</span>115<span>]</span></a></sup><sup id="cite_ref-un2004_118-0" class="reference"><a href="#cite_note-un2004-118"><span>[</span>116<span>]</span></a></sup></caption>
  <tr>
  <th>Region</th>
  <th>1500</th>
  <th>1600</th>
  <th>1700</th>
  <th>1750</th>
  <th>1800</th>
  <th>1850</th>
  <th>1900</th>
  <th>1950</th>
  <th>1999</th>
  <th>2008</th>
  <th>2010</th>
  <th>2012</th>
  <th>2050</th>
  <th>2150</th>
  </tr>
  <tr>
  <th>World</th>
  <td>458</td>
  <td>580</td>
  <td>682</td>
  <td>791</td>
  <td>978</td>
  <td>1,262</td>
  <td>1,650</td>
  <td>2,521</td>
  <td>5,978</td>
  <td>6,707</td>
  <td>6,896</td>
  <td>7,052</td>
  <td>8,909</td>
  <td>9,746</td>
  </tr>
  <tr>
  <th>Africa</th>
  <td>86</td>
  <td>114</td>
  <td>106</td>
  <td>106</td>
  <td>107</td>
  <td>111</td>
  <td>133</td>
  <td>221</td>
  <td>767</td>
  <td>973</td>
  <td>1,022</td>
  <td>1,052</td>
  <td>1,766</td>
  <td>2,308</td>
  </tr>
  <tr>
  <th>Asia</th>
  <td>243</td>
  <td>339</td>
  <td>436</td>
  <td>502</td>
  <td>635</td>
  <td>809</td>
  <td>947</td>
  <td>1,402</td>
  <td>3,634</td>
  <td>4,054</td>
  <td>4,164</td>
  <td>4,250</td>
  <td>5,268</td>
  <td>5,561</td>
  </tr>
  <tr>
  <th>Europe</th>
  <td>84</td>
  <td>111</td>
  <td>125</td>
  <td>163</td>
  <td>203</td>
  <td>276</td>
  <td>408</td>
  <td>547</td>
  <td>729</td>
  <td>732</td>
  <td>738</td>
  <td>740</td>
  <td>628</td>
  <td>517</td>
  </tr>
  <tr>
  <th>Latin America<sup id="cite_ref-Americas_119-0" class="reference"><a href="#cite_note-Americas-119"><span>[</span>Note 1<span>]</span></a></sup></th>
  <td>39</td>
  <td>10</td>
  <td>10</td>
  <td>16</td>
  <td>24</td>
  <td>38</td>
  <td>74</td>
  <td>167</td>
  <td>511</td>
  <td>577</td>
  <td>590</td>
  <td>603</td>
  <td>809</td>
  <td>912</td>
  </tr>
  <tr>
  <th>Northern America<sup id="cite_ref-Americas_119-1" class="reference"><a href="#cite_note-Americas-119"><span>[</span>Note 1<span>]</span></a></sup></th>
  <td>3</td>
  <td>3</td>
  <td>2</td>
  <td>2</td>
  <td>7</td>
  <td>26</td>
  <td>82</td>
  <td>172</td>
  <td>307</td>
  <td>337</td>
  <td>345</td>
  <td>351</td>
  <td>392</td>
  <td>398</td>
  </tr>
  <tr>
  <th>Oceania</th>
  <td>3</td>
  <td>3</td>
  <td>3</td>
  <td>2</td>
  <td>2</td>
  <td>2</td>
  <td>6</td>
  <td>13</td>
  <td>30</td>
  <td>34</td>
  <td>37</td>
  <td>38</td>
  <td>46</td>
  <td>51</td>
  </tr>
  </table>
  '

  library(XML)

  xp <- "//*[text() = 'World historical and predicted populations (in millions)']/ancestor::table"
  popFun <- function(node) {
    x <- xmlValue(node)
    gsub(',', '', x)
  }
  ex2 <- htmltab(doc = html_world, which = xp, bodyFun = popFun)

  expect_that(ex2[1,7], equals("1262"))
})


test_that("Example 3 works", {

  html_bayern <- '<table class="wikitable">
<tr>
<td><span class="flagicon"><a href="/wiki/Spain" title="Spain"><img alt="Spain" src="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/23px-Flag_of_Spain.svg.png" width="23" height="15" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/35px-Flag_of_Spain.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/45px-Flag_of_Spain.svg.png 2x" data-file-width="750" data-file-height="500" /></a></span> <a href="/wiki/Pep_Guardiola" title="Pep Guardiola">Pep Guardiola</a></td>
  <td>Head coach</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Spain" title="Spain"><img alt="Spain" src="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/23px-Flag_of_Spain.svg.png" width="23" height="15" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/35px-Flag_of_Spain.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/45px-Flag_of_Spain.svg.png 2x" data-file-width="750" data-file-height="500" /></a></span> <a href="/wiki/Manuel_Estiarte" title="Manuel Estiarte">Manel Estiarte</a></td>
  <td>Personal assistant</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> <a href="/wiki/Hermann_Gerland" title="Hermann Gerland">Hermann Gerland</a></td>
  <td>Assistant coach</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Spain" title="Spain"><img alt="Spain" src="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/23px-Flag_of_Spain.svg.png" width="23" height="15" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/35px-Flag_of_Spain.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/45px-Flag_of_Spain.svg.png 2x" data-file-width="750" data-file-height="500" /></a></span> Domènec Torrent</td>
  <td>Assistant coach</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> <a href="/wiki/Toni_Tapalovi%C4%87" title="Toni Tapalović">Toni Tapalović</a></td>
  <td>Goalkeeping coach</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Spain" title="Spain"><img alt="Spain" src="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/23px-Flag_of_Spain.svg.png" width="23" height="15" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/35px-Flag_of_Spain.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/45px-Flag_of_Spain.svg.png 2x" data-file-width="750" data-file-height="500" /></a></span> Lorenzo Buenaventura</td>
  <td>Fitness coach</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Andreas Kornmayer</td>
  <td>Fitness coach</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Thomas Wilhelmi</td>
  <td>Fitness coach</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> <a href="/wiki/Matthias_Sammer" title="Matthias Sammer">Matthias Sammer</a></td>
  <td>Sport director</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Spain" title="Spain"><img alt="Spain" src="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/23px-Flag_of_Spain.svg.png" width="23" height="15" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/35px-Flag_of_Spain.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/45px-Flag_of_Spain.svg.png 2x" data-file-width="750" data-file-height="500" /></a></span> Carles Planchart</td>
  <td>Match analyst</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Lars Kornetka</td>
  <td>Video analyst</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Michael Niemeyer</td>
  <td>Video analyst</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> <a href="/wiki/Paul_Breitner" title="Paul Breitner">Paul Breitner</a></td>
  <td>Chief scout</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> <a href="/wiki/Egon_Coordes" title="Egon Coordes">Egon Coordes</a></td>
  <td>Scout</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> <a href="/wiki/Wolfgang_Grobe" title="Wolfgang Grobe">Wolfgang Grobe</a></td>
  <td>Scout</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> <a href="/wiki/Hans-Wilhelm_M%C3%BCller-Wohlfahrt" title="Hans-Wilhelm Müller-Wohlfahrt">Hans-Wilhelm Müller-Wohlfahrt</a></td>
  <td>Chief medic</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Lutz Hänsel</td>
  <td>Team doctor</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Peter Ueblacker</td>
  <td>Team doctor</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Roland Schmidt</td>
  <td>Cardiologist</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Fredi Binder</td>
  <td>Physiotherapist</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Italy" title="Italy"><img alt="Italy" src="//upload.wikimedia.org/wikipedia/en/thumb/0/03/Flag_of_Italy.svg/23px-Flag_of_Italy.svg.png" width="23" height="15" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/0/03/Flag_of_Italy.svg/35px-Flag_of_Italy.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/0/03/Flag_of_Italy.svg/45px-Flag_of_Italy.svg.png 2x" data-file-width="1500" data-file-height="1000" /></a></span> Gianni Bianchi</td>
  <td>Physiotherapist</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Gerry Hoffmann</td>
  <td>Physiotherapist</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Stephan Weickert</td>
  <td>Physiotherapist</td>
  </tr>
  <tr>
  <td><span class="flagicon"><a href="/wiki/Germany" title="Germany"><img alt="Germany" src="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png" width="23" height="14" class="thumbborder" srcset="//upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/35px-Flag_of_Germany.svg.png 1.5x, //upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/46px-Flag_of_Germany.svg.png 2x" data-file-width="1000" data-file-height="600" /></a></span> Helmut Erhard</td>
  <td>Physiotherapist</td>
  </tr>
  </table>'

  xp2 <- "//td[text() = 'Head coach']/ancestor::table"
  ex3 <- htmltab(doc = html_bayern, which = xp2, header = 0, encoding = "UTF-8", colNames = c("name", "role"))

  expect_that(ex3[1,1], equals("Pep Guardiola"))
  expect_that(colnames(ex3)[1], equals("name"))
})


test_that("Example 4 works", {

  html_web <- '<table class="wikitable" style="text-align:right;">
<caption><b>Global desktop and mobile stats combined from <a rel="nofollow" class="external text" href="http://gs.statcounter.com/#all-browser-ww-monthly-200812-201502">StatCounter</a> (Top 5 browsers)</b></caption>
<tr>
  <th scope="col" style="width:8em" rowspan="2">Date<br /></th>
  <th scope="col" style="width:4em" rowspan="2"><a href="/wiki/Internet_Explorer" title="Internet Explorer">Internet<br />
  Explorer</a> <sup id="cite_ref-DesktopMobile_17-0" class="reference"><a href="#cite_note-DesktopMobile-17"><span>[</span>Note 1<span>]</span></a></sup></th>
  <th scope="col" style="width:4em" rowspan="2"><a href="/wiki/Google_Chrome" title="Google Chrome">Chrome</a></th>
  <th scope="col" style="width:4em" rowspan="2"><a href="/wiki/Firefox" title="Firefox">Firefox</a></th>
  <th scope="col" colspan="3"><a href="/wiki/Safari_(web_browser)" title="Safari (web browser)">Safari</a></th>
  <th scope="col" colspan="3"><a href="/wiki/Opera_(web_browser)" title="Opera (web browser)">Opera</a></th>
  <th scope="col" style="width:4em" rowspan="2"><a href="/wiki/Android_(operating_system)" title="Android (operating system)">Android</a></th>
  <th scope="col" style="width:4em" rowspan="2"><a href="/wiki/Mobile_browser" title="Mobile browser">Mobile<br />
  Total</a></th>
  </tr>
  <tr>
  <th scope="col" style="width:3em">Desktop+Laptop</th>
  <th scope="col" style="width:3em">Mobile</th>
  <th scope="col" style="width:3em">Total</th>
  <th scope="col" style="width:3em">Desktop+Laptop</th>
  <th scope="col" style="width:3em">Mobile</th>
  <th scope="col" style="width:3em">Total</th>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201307-201307-bar">July 2013</a></th>
  <td>20.27%</td>
  <td>36.29%</td>
  <td>16.60%</td>
  <td>7.10%</td>
  <td>4.27%</td>
  <td>11.37%</td>
  <td>1.01%</td>
  <td>2.73%</td>
  <td>3.74%</td>
  <td>4.97%</td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201307-201307-bar">17.35%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201306-201306-bar">June 2013</a></th>
  <td>21.35%</td>
  <td>35.82%</td>
  <td>16.79%</td>
  <td>7.04%</td>
  <td>3.79%</td>
  <td>10.83%</td>
  <td>1.03%</td>
  <td>2.28%</td>
  <td>3.31%</td>
  <td>4.66%</td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201306-201306-bar">16.08%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201305-201305-bar">May 2013</a></th>
  <td>23.67%</td>
  <td>35.66%</td>
  <td>16.87%</td>
  <td>6.80%</td>
  <td>3.67%</td>
  <td>10.55%</td>
  <td>0.87%</td>
  <td>2.13%</td>
  <td>3.00%</td>
  <td>4.45%</td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201305-201305-bar">14.62%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201304-201304-bar">April 2013</a></th>
  <td>25.58%</td>
  <td>33.71%</td>
  <td>17.27%</td>
  <td>6.89%</td>
  <td>3.76%</td>
  <td>10.65%</td>
  <td>0.85%</td>
  <td>2.26%</td>
  <td>3.11%</td>
  <td>4.30%</td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201304-201304-bar">13.90%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201303-201303-bar">March 2013</a></th>
  <td>25.08%</td>
  <td>32.88%</td>
  <td>17.86%</td>
  <td>7.28%</td>
  <td>3.90%</td>
  <td>11.18%</td>
  <td>1.00%</td>
  <td>2.24%</td>
  <td>3.24%</td>
  <td>4.43%</td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201303-201303-bar">14.44%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201302-201302-bar">February 2013</a></th>
  <td>25.54%</td>
  <td>31.96%</td>
  <td>18.28%</td>
  <td>7.37%</td>
  <td>3.89%</td>
  <td>11.26%</td>
  <td>1.04%</td>
  <td>2.21%</td>
  <td>3.25%</td>
  <td>4.53%</td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201302-201302-bar">14.35%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201301-201301-bar">January 2013</a></th>
  <td>26.37%</td>
  <td>31.51%</td>
  <td>18.39%</td>
  <td>7.12%</td>
  <td>3.64%</td>
  <td>10.76%</td>
  <td>1.02%</td>
  <td>2.17%</td>
  <td>3.19%</td>
  <td>4.36%</td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201301-201301-bar">14.13%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right; border-top: 2px solid black;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201207-201207-bar">July 2012</a></th>
  <td style="border-top: 2px solid black;">28.49%</td>
  <td style="border-top: 2px solid black;">30.06%</td>
  <td style="border-top: 2px solid black;">21.01%</td>
  <td style="border-top: 2px solid black;">6.33%</td>
  <td style="border-top: 2px solid black;">2.77%</td>
  <td style="border-top: 2px solid black;">9.10%</td>
  <td style="border-top: 2px solid black;">1.53%</td>
  <td style="border-top: 2px solid black;">2.15%</td>
  <td style="border-top: 2px solid black;">3.68%</td>
  <td style="border-top: 2px solid black;">2.57%</td>
  <td style="border-top: 2px solid black;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201207-201207-bar">11.09%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201201-201201-bar">January 2012</a></th>
  <td>34.27%</td>
  <td>25.99%</td>
  <td>22.68%</td>
  <td>6.06%</td>
  <td>1.92%</td>
  <td>7.98%</td>
  <td>1.78%</td>
  <td>2.06%</td>
  <td>3.84%</td>
  <td>1.71%</td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201201-201201-bar">8.49%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201107-201107-bar">July 2011</a></th>
  <td>39.47%</td>
  <td>20.59%</td>
  <td>25.99%</td>
  <td>4.81%</td>
  <td>1.40%</td>
  <td>6.21%</td>
  <td>1.54%</td>
  <td>1.54%</td>
  <td>3.08%</td>
  <td>1.28%</td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201107-201107-bar">7.02%</a></td>
  </tr>
  <tr>
  <th scope="row" style="text-align:right;"><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#browser-ww-monthly-201106-201106-bar">June 2011</a></th>
  <td>40.73%</td>
  <td>19.30%</td>
  <td>26.49%</td>
  <td></td>
  <td>4.74%</td>
  <td></td>
  <td></td>
  <td>1.63%</td>
  <td></td>
  <td></td>
  <td><a rel="nofollow" class="external text" href="http://gs.statcounter.com/#mobile_vs_desktop-ww-monthly-201106-201106-bar">6.53%</a></td>
  </tr>
  </table>'

  bFun <- function(node) {
    x <- XML::xmlValue(node)
    x <- gsub('%$', '', x)
    ifelse(x == '', NA, x)
  }
  ex4 <- htmltab(doc = html_web, which = 1, bodyFun = bFun)

  expect_that(ex4[1,1], equals("July 2013"))
  expect_that(ex4[1,2], equals("20.27"))
  expect_that(is.na(ex4[11,5]), is_true())
})
