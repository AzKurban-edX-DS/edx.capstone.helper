kaggle_cli.download <- function(dataset.path, data.local_path, unzip = FALSE) {
  if (system("kaggle --version", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop("Kaggle CLI is not installed or not in the PATH.")
  }

  if (system(trimws(paste("kaggle datasets download", kaggle_dataset, "--path",
                          data.local_path, ifelse(unzip, "--unzip", "")))) != 0) {
    stop(get_log1("Failed to download the dataset with Kaggle CLI: `%1`.", dataset.path))
  }

  if(!dir.exists(data.local_path)) {
    stop(get_log1("Failed to download and unzip the Kaggle dataset: `%1`.", dataset.path))
  }
}

img.file_path.get_list <- function(root_path, folder.list = NULL) {
  if(is.null(folder.list))
    folder.list <- dir(root_path)

  path_list <- lapply(folder.list, function(folder_name){
    file.root_path <- file.path(root_path, folder_name)
    list(root_path = file.root_path,
         file_path.list = list.files(file.root_path,
                                     full.names = TRUE))
  })

  names(path_list) <- folder.list |> substr(1,1)
  path_list
}

load.kaggle_img <- function(file,
                            resize_x = 28,
                            resize_y = 28) {
  imager::load.image(file) |>
    grayscale() |>
    resize(size_x = resize_x,
           size_y = resize_y)
}

as.matrix.cimg <- function(cimg.list,
                           size_x = 28,
                           size_y = 28,
                           label) {
  mx.ncols <- size_x*size_y

  map(cimg.list, function(cimg) {
    as.vector(cimg[,,1,1])
  }) |> unlist() |>
    matrix(ncol = mx.ncols,
           byrow = TRUE,
           dimnames = list(base::rep(label, times = length(cimg.list)),
                           1:mx.ncols))
}

char.image <- function(char.vector,
                       mx.nrow = 28,
                       mx.ncol = 28) {
  image(matrix(char.vector, nrow = mx.nrow)[, mx.ncol:1])
}

create.hwChar_dataset <- function(root_path, folder.list = NULL){
  start <- put_start_date()
  put_log("Getting file path lists...")
  img.file_list <- img.file_path.get_list(root_path, folder.list)
  put_end_date(start)
  put_log("File path lists have been created")
  put(str(img.file_list))

  start <- put_start_date()
  put_log("Loading image files...")
  img_list <- lapply(img.file_list, function(img_f){
    list(cimg.list = map_il(img_f$file_path.list, load.kaggle_img),
         fpath.list = img_f$file_path.list)
  })
  put_end_date(start)
  put_log("Image files have been loaded.")
  put(str(img_list))

  start <- put_start_date()
  put_log("Converting image lists to matrices...")
  char_matrix.list <- lapply(names(img_list), function(label){
    img_list[[label]]$cimg.list |>
      as.matrix.cimg(label)
  })
  put_end_date(start)
  put_log("Image matrix list has been created.")
  put(str(char_matrix.list))

  start <- put_start_date()
  put_log("Combining image data to single matrix...")
  img.mx <- do.call(rbind, char_matrix.list)
  put_end_date(start)
  put_log("Image dataset matrix has been created.")
  put(dim(img.mx))

  list(img.files = img.file_list,
       img.list = char_matrix.list,
       my_mnist = img.mx)
}
