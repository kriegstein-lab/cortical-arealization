//srcdir = getDirectory("Choose a Directory...");
srcdir = "/Users/carmensandoval/Desktop/imageJ_test/";

git_candidate_sha = File.openAsString("/Users/carmensandoval/Documents/GitHub/permanentheaddamagePHD/.git/HEAD");
// git_head_ref can contain either a git sha, or point to a file with the sha
// Check to see if there is a slash, indicating it is a file, and get the sha
// from that file
if (indexOf(git_candidate_sha, "/") != -1) {
  // If it is a file, it will be something like 'ref: refs/heads/master\n'
  // Remove 'ref: '
  git_candidate_sha = substring(git_candidate_sha, 5);
  // Remove '\n'
  git_candidate_sha = substring(git_candidate_sha, 0, lengthOf(git_candidate_sha) - 1);
  // Read file to get sha
  git_candidate_sha = File.openAsString("/Users/carmensandoval/Documents/GitHub/permanentheaddamagePHD/.git/" + git_candidate_sha);
}
// Truncate to get first 7 digits of the sha
git_sha = substring(git_candidate_sha, 0, 7);
dstdir = "/Users/carmensandoval/Box/KriegsteinLab/imaging/Fgf8_Organoids/processed/" + git_sha + "/";
if (File.exists(dstdir)) {
  exit("Directory already exists!");
}
File.makeDirectory(dstdir);

setBatchMode(true);
processDirectory(srcdir, dstdir);

function processDirectory(srcdir, dstdir) {
  filelist = getFileList(srcdir);

  for (i = 0; i< filelist.length; i++) {
    src_filename = srcdir + filelist[i];
    dst_filename = dstdir + filelist[i];
    if (File.isDirectory(src_filename)) {
      File.makeDirectory(dst_filename);
      processDirectory(src_filename, dst_filename);
    } 
    else if (endsWith(src_filename, "tif")) {
	  if (indexOf(src_filename, "ch00") != -1) {
        processChan00(src_filename, dst_filename);
      } 
      else if (indexOf(src_filename, "ch01") != -1) {
      	//processChan01(src_filename, dst_filename);
      }
      else if (indexOf(src_filename, "ch02") != -1) {
		//processChan02(src_filename, dst_filename);
	  } 
	  else if (indexOf(src_filename, "ch03") != -1) {
        //processChan03(src_filename, dst_filename);
	  }
      close("*");
    }
  }
}

function openAndScale(filename) {
  open(filename);
  run("Scale...", "x=0.6 y=0.6 width="+getWidth() * 0.6+" height="+getHeight() * 0.6+"\
    interpolation=Bicubic average create");
}

function getBaseFilename(filename) {
  return replace(filename, ".tif", "");
}

function saveTif(dst_filename) {
  //baseFileName = getBaseFilename(dst_filename);
  //saveAs("TIFF", baseFileName + "_scaled");
}

function savePng(dst_filename) {
  baseFileName = getBaseFilename(dst_filename);
  saveAs("PNG", baseFileName + "edit");
}

function processChan00(src_filename, dst_filename) {
  openAndScale(src_filename);
  saveTif(dst_filename);
  
  run("Brightness/Contrast...");
  call("ij.ImagePlus.setDefault16bitRange", 12);
  setMinAndMax(0, 4095);
  run("mpl-inferno");
  setMinAndMax(90, 1150);
  
  savePng(dst_filename);
}

function processChan01(src_filename, dst_filename) {
  openAndScale(src_filename);
  saveTif(dst_filename);
	
  // Process here
	
  savePng(dst_filename);
}

function processChan02(src_filename, dst_filename) {
  openAndScale(src_filename);
  saveTif(dst_filename);
	
  // Process here
	
  savePng(dst_filename);
}

function processChan03(src_filename, dst_filename) {
  openAndScale(src_filename);
  saveTif(dst_filename);
	
  // Process here
	
  savePng(dst_filename);
}