/* dependencies:
 *   - jquery.min.js
 *   - abcjs_basic_3.0-min.js
 */

// Global Variables Dictionary 
glob = {'path_to_jazz_orig': '/assets/jazzData_orig.json',
        'path_to_user_jazz': '/assets/userCreated/jazzData.txt',
        'music': null,
        'musicID': null,
        'lists': null,
        'collections': null,
        'all': null,
        'fileContent': null};

// Read jazzData immediately
if (glob.collections === null ) { 
  readFile(path=glob.path_to_jazz_orig, var_name='all');
}

$('#main-navbar li').on('click', function () {
    $('#main-navbar .active').attr("class", ""); 
    $('#'+this.id).attr("class", "active")
});

function transpose(key) {
  glob.music = util.transpose(glob.music, key);
  $('#musicID').text(glob.musicID + ` (in ${key})`)
  ABCJS.renderAbc('music', glob.music);
}

function filterMusic(type, name) {
  var out = glob.all.filter(function(x) {return type==x.type && name==x.name});
  if (out.length == 0) {
    out = ""
  } else {
    out = out[0].music
  }
  return out
}

function showMusic(type, name) {
  glob.music = '%%staffwidth 500\n' + filterMusic(type, name);
  glob.musicID = name + " " + type
  console.log(glob.music)
  $('#musicID').text(name + " " + type)
  ABCJS.renderAbc('music', glob.music);
}


function genMenuItem(item) {
  return `
    <li role="presentation"><a role="menuitem" tabindex="-1" 
        onclick='showMusic("${item.type}","${item.name}")'>
      ${item.name + " " + item.type}
    </a></li>
  `;
}

$('#all').on('click', function () {
  if ($('#all ul').length == 0) {
    $('#all').append('<ul class="dropdown-menu"></ui>')
    for (var i in glob.all) {
      var item = glob.all[i]
      $('#all ul').append(genMenuItem(item))
    }
  }
});

$('#transpose').on('click', function () {
  if ($('#transpose ul').length == 0) {
    $('#transpose').append('<ul class="dropdown-menu"></ui>')
    for (var i in util.keySigs) {
      var key = util.keySigs[i]
      $('#transpose ul').append(`
        <li role="presentation"><a role="menuitem" tabindex="-1" 
            onclick='transpose("${key}")'>
            ${key}
        </a></li>`)
    }
  }
});



function fillTextArea(music) {
  $('#edit').val(music);
}

function editMusic() {
  //document.getElementById('edit').setAttribute('style', 'display:inline');
  $('#edit').setAttribute('style', 'display:inline');
}

function quitEditMusic() {
  $('#edit').setAttribute('style', 'display:none');
}

function getMusic() {
  return $('#edit').val();
}

function renderMusic() {
  var staffWidth = $('#edit').width() * 0.75;
  var music = '%%staffwidth ' + staffWidth + '\n' + getMusic();
  console.log(music)
  ABCJS.renderAbc('music', music);
}

function readFile(path=glob.path_to_jazz_orig, var_name='fileContent') {
  $.ajax({
    type: 'GET',
    url: path,
    dataType: "json", 
    success: function(dat) {
      glob[var_name] = dat.jazzData 
    }
  });
}

function writeFile(path=glob.path_to_user_jazz, var_name='fileContent') {
  $.ajax({ url: path, success: function(file_content) {
      glob[var_name] = file_content;
    }
  });
}

function showExample() {
  readFile();
  fillTextArea(glob.fileContent);
}


/*  500: 695
 *  600: 828
 *  700: 961
 *  800: 1094
 *  900: 1227
 * 1000: 1360
 */
