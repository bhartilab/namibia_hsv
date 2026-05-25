# Function to provide feedback in the console
note <- function(X){
		cat(X)
		flush.console()
}

# Define path.workspace as the path to the folder where the eps will be saved
# All the necessary packages are explicitly called next to every function as necessary_package::function
# Make sure all the necessary packages are installed first
# The necessary data sets (not provided) are hsv_test, net2015, and net2016

figure_2 <- function(palette="C", num_col=10){
	graph_net <- function(year_s=2015, palette_v=palette, show_legend=FALSE){
		hsv_test$cat[hsv_test$id %in% c(99, 111)] <- "F"

		num_row_F_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="F"])) %/% num_col
		rem_F_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="F"])) %% num_col
		num_row_M_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="M"])) %/% num_col
		rem_M_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="M"])) %% num_col
		num_row_young_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="young"])) %/% num_col
		rem_young_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="young"])) %% num_col

		num_row_F_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="F"])) %/% num_col
		rem_F_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="F"])) %% num_col
		num_row_M_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="M"])) %/% num_col
		rem_M_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="M"])) %% num_col
		num_row_young_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="young"])) %/% num_col
		rem_young_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="young"])) %% num_col

		num_row_2015 <- num_row_F_2015+ifelse(rem_F_2015>0, 1, 0)+num_row_M_2015+ifelse(rem_M_2015>0, 1, 0)+num_row_young_2015+ifelse(rem_young_2015>0, 1, 0)
		num_row_2016 <- num_row_F_2016+ifelse(rem_F_2016>0, 1, 0)+num_row_M_2016+ifelse(rem_M_2016>0, 1, 0)+num_row_young_2016+ifelse(rem_young_2016>0, 1, 0)
		row_max <- max(c(num_row_2015, num_row_2016))

		if(year_s==2015){
			rem_F <- rem_F_2015
			rem_M <- rem_M_2015
			rem_young <- rem_young_2015
			num_row_F <- num_row_F_2015
			num_row_M <- num_row_M_2015
			num_row_young <- num_row_young_2015
		}else{
			rem_F <- rem_F_2016
			rem_M <- rem_M_2016
			rem_young <- rem_young_2016
			num_row_F <- num_row_F_2016
			num_row_M <- num_row_M_2016
			num_row_young <- num_row_young_2016
		}
		coord <- rbind(
					hsv_test %>%
						dplyr::filter(year==year_s & cat=="F") %>%
						dplyr::select(id, cat) %>%
						unique() %>%
						dplyr::arrange(id) %>%
						dplyr::mutate(
							x=c(rep(1:num_col, num_row_F), 1:rem_F),
							y=c(rep(1:num_row_F, each=num_col), rep((num_row_F+1), rem_F))),
					hsv_test %>%
						dplyr::filter(year==year_s & cat=="M") %>%
						dplyr::select(id, cat) %>%
						unique() %>%
						dplyr::arrange(id) %>%
						dplyr::mutate(
							x=c(rep(1:num_col, num_row_M), 1:rem_M),
							y=c(rep(1:num_row_M, each=num_col), rep((num_row_M+1), rem_M))),
					hsv_test %>%
						dplyr::filter(year==year_s & cat=="young") %>%
						dplyr::select(id, cat) %>%
						unique() %>%
						dplyr::arrange(id) %>%
						dplyr::mutate(
							x=c(rep(1:num_col, num_row_young), 1:rem_young),
							y=c(rep(1:num_row_young, each=num_col), rep((num_row_young+1), rem_young)))
				)
		coord$y[coord$cat=="M"] <- coord$y[coord$cat=="M"]+max(coord$y[coord$cat=="F"])
		coord$y[coord$cat=="young"] <- coord$y[coord$cat=="young"]+max(coord$y[coord$cat=="M"])
		coord$y <- (row_max+1)-coord$y

		col_code <- function(x, legend=FALSE){
			cols <- viridis::viridis(3, option=palette_v)
			data_prep <- function(x){
				col <- ifelse(unique(hsv_test$cat[hsv_test$id==x])=="F", cols[2],
							ifelse(unique(hsv_test$cat[hsv_test$id==x])=="M", cols[1], cols[3]))
				temp <- hsv_test[hsv_test$id==x,] %>%
							dplyr::mutate(sample=factor(sample,
									levels = rev(c(2, 4, 3, 1))),
								val=0.25,
								col=ifelse(genome==1, col, "white")) %>%
							dplyr::select(-dna) %>%
							dplyr::select(-cat) %>%
							dplyr::select(-year)
				if(unique(hsv_test$cat[hsv_test$id==x])=="M"){
					fill <- data.frame(
								sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
								genome=0,
								id=as.integer(x),
								val=0.25,
								col="grey") %>%
							dplyr::filter(sample != "4")
					miss <- !(fill$sample %in% temp$sample)
					temp <- dplyr::bind_rows(temp, fill[miss,])
				}else{
					if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=as.integer(x),
									val=0.25,
									col="grey") %>%
								dplyr::filter(sample == "1")
						miss <- !(fill$sample %in% temp$sample)
						temp <- rbind(temp, fill[miss,])
					}else{
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=as.integer(x),
									val=0.25,
									col="grey")
						miss <- !(fill$sample %in% temp$sample)
						temp <- dplyr::bind_rows(temp, fill[miss,])
					}
				}
		
				cols <- temp$col[order(as.integer(temp$sample))]
				return(list(temp, cols))
			}
			if(!legend){
				temp_d <- data_prep(x)
				temp <- temp_d[[1]]
				cols <- temp_d[[2]]
	
				pie <- ggplot2::ggplot(temp) +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.75) +
							ggplot2::coord_polar("y", start=0) +
							ggplot2::scale_fill_manual(values=cols) +
							ggplot2::theme_void() +
							ggplot2::theme(legend.position = "none")
				if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col=NA)
				}else{
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.5)
				}
				return(pie)
			}else{
				legend_1 <- data_prep("118")
				legend_2 <- data_prep("21")
				legend_3 <- data_prep("170")
	
				legend_t <- rbind(
								legend_1[[1]] %>%
									dplyr::mutate(
										title="F",
										col="black"),
								legend_2[[1]] %>%
									dplyr::mutate(
										title="M",
										col="black"),
								legend_3[[1]] %>%
									dplyr::mutate(
										title="Child",
										col=NA)) %>%
							dplyr::mutate(
								fill=c(legend_1[[2]], legend_2[[2]], legend_3[[2]]),
								order=factor(c(2, 3, 4, 1, 7, 8, 6, 9), levels=c(2, 3, 4, 1, 7, 8, 6, 9))) %>%
							dplyr::mutate(samp_pos=sample)
	
				# Tweaking the legend for M
				legend_t$sample <- as.integer(as.character(legend_t$sample))
				legend_t$sample[legend_t$title=="M"] <- c(1,3,2)
				legend_t <- legend_t %>%
							dplyr::mutate(sample=factor(sample, levels = rev(c(2, 4, 3, 1)))) %>%
							dplyr::group_by(title) %>%
							dplyr::arrange(title, as.character(sample)) %>%
							dplyr::mutate(val=1/dplyr::n()) %>%
							dplyr::mutate(pos=val/2 + c(0, cumsum(val)[-length(val)])) %>%
							dplyr::ungroup() %>%
							data.frame()
				legend_t$samp_pos <- as.character(legend_t$samp_pos)
				legend_t$samp_pos[legend_t$title=="M"] <- c("2", "3", "1")
				legend_t$samp_pos[legend_t$title=="F"] <- c("2", "4", "3", "1")
	
				legend_t$title <- factor(
									legend_t$title,
									levels = c("F", "M", "Child"))
	
				pie <- ggplot2::ggplot(legend_t) +
						ggplot2::coord_polar("y") +
						ggplot2::facet_wrap(.~title, ncol=1) +
						ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=I(fill)),
							# fill="white",
							stat="identity",
							color="black",
							size=0.75) +
						ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=I(fill), color=I(col)),
							# fill="white",
							stat="identity",
							size=0.5) +
						ggplot2::geom_text(
							ggplot2::aes(x=0, y = pos, label = samp_pos),
							size=8) +
						ggplot2::theme_void() +
						ggplot2::theme(
							legend.position = "none",
							strip.text.x = element_text(size = 22, face = "bold", hjust=0.5))
			
				legend_df <- data.frame(
								y=c(1.5, 5.5, 9.5),
								label=c("Not tested", "No HSV", "HSV")) %>%
								dplyr::mutate(x=2)
				poly1 <- matrix(
				  c(-1.2, (0.2+2/3), 1.2, (0.2+2/3),
				    1.2, 0, -1.2, 0,
				    -1.2, (0.2+2/3)),
				  ncol=2, byrow=TRUE) %>%
				  as.data.frame() %>%
				  setNames(., c("x", "y")) %>%
				  sf::st_as_sf(coords = c("x", "y")) %>%
				  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
				  sf::st_cast("POLYGON")

				poly2 <- matrix(
				  c(-1.2, (0.2+4/3), 1.2, (0.2+4/3),
				    1.2, 2.6, -1.2, 2.6,
				    -1.2, (0.2+4/3)),
				  ncol=2, byrow=TRUE) %>%
				  as.data.frame() %>%
				  setNames(., c("x", "y")) %>%
				  sf::st_as_sf(coords = c("x", "y")) %>%
				  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
				  sf::st_cast("POLYGON")

				poly3 <- matrix(
				  c(-1.2, (0.2+4/3), 1.2, (0.2+4/3),
				    1.2,(0.2+2/3), -1.2,(0.2+2/3),
				    -1.2, (0.2+4/3)),
				  ncol=2, byrow=TRUE) %>%
				  as.data.frame() %>%
				  setNames(., c("x", "y")) %>%
				  sf::st_as_sf(coords = c("x", "y")) %>%
				  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
				  sf::st_cast("POLYGON")

				legend_df <- data.frame(
				                y=c(1.5, 5.5, 9.5),
				                label=c("Not tested", "No HSV", "HSV")) %>%
				                dplyr::mutate(x=2)

				legend_circle <- data.frame(x=0, y=1.2) %>%
				      sf::st_as_sf(coords = c('x', 'y')) %>%
				      dplyr::mutate(geometry = sf::st_buffer(geometry, 1))

				legend_circle_s <- rbind(
				              sf::st_intersection(legend_circle, poly2),
				              sf::st_intersection(legend_circle, poly3),
				              sf::st_intersection(legend_circle, poly1))

				legend_circle_s <- legend_circle_s %>%
				        dplyr::mutate(id=1:3 %>% as.character())
				pie <- cowplot::ggdraw(
							pie +
								ggplot2::theme(
									plot.margin=ggplot2::margin(0, 14, 0, 0, unit="cm"))) +
						cowplot::draw_grob(
							ggplot2::ggplotGrob(
								ggplot2::ggplot() +
					              ggplot2::geom_sf(
					                data=legend_circle_s,
					                ggplot2::aes(fill=as.character(id))) +
					              ggplot2::scale_fill_manual(
					                name="",
					                values=c(
					                  "1"="#0D0887FF",
					                  "2"="#CC4678FF",
					                  "3"="#F0F921FF")) +
					              ggplot2::geom_text(
					                ggplot2::aes(x=1.5, y=1.2),
					                label="HSV detected",
					                hjust=0,
					                size=10) +
					              ggplot2::xlim(-1.2, 8) +
								    ggplot2::theme_void() +
								    ggplot2::theme(legend.position="none")), 
								0.35,
								0.6,
								0.65, 
								0.15) +
						cowplot::draw_grob(
							ggplot2::ggplotGrob(
								ggplot2::ggplot() +
									ggplot2::geom_sf(
										data=legend_circle,
										fill="white",
										color="black") +
									ggplot2::geom_text(
										ggplot2::aes(x=1.5, y=1.2),
										label="HSV not detected",
										hjust=0,
										size=10) +
									ggplot2::xlim(-1.2, 8) +
									ggplot2::theme_void()), 
								0.35,
								0.4,
								0.65, 
								0.15)  +
						cowplot::draw_grob(
							ggplot2::ggplotGrob(
								ggplot2::ggplot() +
									ggplot2::geom_sf(
										data=legend_circle,
										fill="grey",
										color="black") +
									ggplot2::geom_text(
										ggplot2::aes(x=1.5, y=1.2),
										label="Sample not collected",
										hjust=0,
										size=10) +
									ggplot2::xlim(-1.2, 8) +
									ggplot2::theme_void()), 
								0.35,
								0.2,
								0.65, 
								0.15)
				return(pie)
			}
		}

		if(show_legend){
			legend_fig <- col_code(legend=TRUE)
			return(legend_fig)
		}else{
			subplots <- lapply(coord$id, col_code, legend=FALSE)
			
			frame <- ggplot2::ggplot() +
						ggplot2::geom_point(
							data=coord,
							ggplot2::aes(x, y),
							color=NA) +
						ggplot2::ylim(-2, row_max) +
						ggplot2::ggtitle(paste0("Samples collected in ", year_s)) +
						ggplot2::theme_void() +
						ggplot2::theme(
							plot.title = element_text(size=16, hjust=0.5))
		
			for(i in 1:length(subplots)){
				frame <- frame +
							ggplot2::annotation_custom(
								ggplot2::ggplotGrob(subplots[[i]]), 
									x = (coord$x[i]-0.45),
									y = (coord$y[i]-0.45), 
									xmax = (coord$x[i]+0.45),
									ymax = (coord$y[i]+0.45))
			}
			frame <- frame +
						ggplot2::geom_text(
							data=coord,
							ggplot2::aes(
								x=x,
								y=y-0.48,
								label=id),
							size=2,
							fontface="bold")
			return(frame)
		}
	}

	ggplot2::ggsave(
		filename=file.path(path.workspace, "figure_2_Namibia.eps"),
		cowplot::plot_grid(
			graph_net(year_s=2015, show_legend=FALSE),
			graph_net(year_s=2016, show_legend=FALSE),
			ncol=1
		),
		width=7, height=16
	)
	ggplot2::ggsave(
		filename=file.path(path.workspace, "figure_2_Namibia_legend.eps"),
		graph_net(year_s=2016, show_legend=TRUE), 
		width=14, height=6
		)
	note("Figure saved...\n")
}

figure_5  <- function(palette="C"){
	graph_net <- function(year=2015, palette_v=palette){
			if(year==2016){
				net <- net2016
			}else{
				net <- net2015
			}
			data <- net_d_manage(net, verbose=FALSE)
			nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
		
			if(year==2016){
				edges <- edge2016
				nodes <- nodes[!(nodes$id==216 & nodes$phone_own_duration==24),] %>%
							dplyr::filter(!(id==214 & adult_house==4)) %>%
							dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
									ifelse(phone_own>0, "Phone", "No phone")),
								phone_compound=ifelse(is.na(other_phone_own), "No data",
									ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
							dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
									ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
									phone_c2=ifelse(phone=="Phone", "Phone owned",
									ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
										ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
							dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
									phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
										ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
							dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
							dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
							dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
							dplyr::mutate(sex=ifelse(is.na(sex), NA,
								ifelse(sex==2, "F", "M")),
								hsv=ifelse(id %in% hsv_test$id[hsv_test$year==2016 & hsv_test$genome==1], "Positive", "Negative"))
		
				links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
				links$min <- apply(links[,1:2],1,min)
				links$max <- apply(links[,1:2],1,max)
				links <- links[links$min!=links$max,]
				links$comb <- paste(links$min, links$max,sep="-")
				links <- links[!duplicated(links$comb),c("x","y")]
				links_l <- links
				links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
									"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
									(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
										"Phone", "No Phone")
				nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
										nodes$id %in% nodes$id[nodes$phone=="Phone"],
										"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
									(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
										"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
									(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
										"Phone", "No Phone")
				nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
										nodes$id %in% nodes$id[nodes$phone=="Phone"],
										"Phone", "No Phone")
			}else{
				edges <- edge2015
				nodes <- nodes %>%
							dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
									ifelse(phone_own>0, "Phone", "No phone")),
								phone_compound=ifelse(is.na(other_phone_own), "No data",
									ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
							dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
									ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
									phone_c2=ifelse(phone=="Phone", "Phone owned",
									ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
										ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
							dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
							dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))
		
				links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
				links$min <- apply(links[,1:2],1,min)
				links$max <- apply(links[,1:2],1,max)
				links <- links[links$min!=links$max,]
				links$comb <- paste(links$min, links$max,sep="-")
				links <- links[!duplicated(links$comb),c("x","y")]
				l_list <- unique(c(links$x, links$y))
				temp <- net[[2]][, c("id", "age", "sex")] %>%
							dplyr::mutate(id=as.integer(id))
				l_list <- data.frame(id=as.integer(l_list[!(l_list %in% as.character(nodes$id))])) %>% # List of ID in the file for children but not in the other ones
							dplyr::left_join(., temp, by="id")
				nodes <- nodes %>%
								dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
								dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
				nodes[match(l_list$id, nodes$id), c("age", "sex")] <- l_list[na.omit(match(nodes$id, l_list$id)), c("age", "sex")]
				nodes <- nodes %>%
							dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
							dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
							dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
								phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
										ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
							# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
							dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
							dplyr::mutate(sex=ifelse(is.na(sex), NA,
								ifelse(sex==2, "F", "M")),
								hsv=ifelse(id %in% hsv_test$id[hsv_test$year==2015 & hsv_test$genome==1], "Positive", "Negative"))
		
				links_l <- links
				links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
									"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
									(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
										"Phone", "No Phone")
				nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
										nodes$id %in% nodes$id[nodes$phone=="Phone"],
										"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
									(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
										"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
									(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
										"Phone", "No Phone")
				nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
										nodes$id %in% nodes$id[nodes$phone=="Phone"],
										"Phone", "No Phone")
			}
		
			edges <- setNames(edges, c("x", "y"))
			edges$link <- unlist(lapply(1:nrow(edges), function(x){
				paste(sort(t(edges)[,x]), collapse="-")
			}))
			links$link <- unlist(lapply(1:nrow(links), function(x){
				paste(sort(t(links)[,x]), collapse="-")
			}))
			links_df <- rbind(edges, links) %>%
							dplyr::group_by(link) %>%
							dplyr::mutate(n=dplyr::row_number()) %>%
							dplyr::ungroup() %>%
							dplyr::filter(n==1) %>%
							dplyr::select(x, y, link)
			links_df$x <- ifelse(substr(links_df$x, 1, 1)=="0", substr(links_df$x, 2, nchar(links_df$x)), links_df$x)
			links_df$y <- ifelse(substr(links_df$y, 1, 1)=="0", substr(links_df$y, 2, nchar(links_df$y)), links_df$y)
	
			added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))
		
			net <- igraph::graph_from_data_frame(
						d=unique(links_df[,c("x", "y")]),
						vertices=data.frame(id=added_id),
						directed=FALSE)
			gnet <- intergraph::asNetwork(igraph::simplify(net))
		
			d_dist <- data.frame(
						id=added_id,
						d=igraph::degree(net)) %>%
						dplyr::left_join(
							.,
							nodes %>%
								dplyr::mutate(
									id=as.character(id)) %>%
								dplyr::select(id, hsv),
							by="id") %>%
						dplyr::mutate(hsv=ifelse(is.na(hsv), "Sample not collected", hsv)) %>%
						dplyr::mutate(hsv=factor(hsv,
										levels=c("Sample not collected", "Negative", "Positive"),
										labels=c("Sample not collected", "HSV not detected", "HSV detected")))
	
			color_code <- viridis::viridis(3, option="G")
			degree_dist <- ggplot2::ggplot(
							d_dist,
							ggplot2::aes(x=d, fill=hsv)) +
							ggplot2::geom_bar(stat="count") +
							ggplot2::scale_fill_manual(
								name="",
								values=c("grey", "black", "steelblue")) +
							ggplot2::xlab("Number of edges") +
							ggplot2::ylab("Count") +
							ggplot2::guides(
								fill = ggplot2::guide_legend(override.aes = list(size = 4))) +
							ggplot2::theme_bw() +
							ggplot2::xlim(c(-0.5, 12.5)) +
							ggplot2::ggtitle(paste0(nrow(d_dist), " individuals in the network in ", year, " (", nrow(d_dist[d_dist$hsv!="Sample not collected",]), " participants)")) +
							ggplot2::theme(
								legend.position = legend_position,
								legend.box.background = element_rect(colour = "black"),
								legend.title = ggplot2::element_text(size = 6),
								legend.text  = ggplot2::element_text(size = 9),
								legend.key.size = ggplot2::unit(0.5, "lines"),
								axis.title=ggplot2::element_text(size=8, face="bold"),
								plot.title = element_text(size=9))
		
			links_df$part <- ifelse(links_df$link %in% edges$link, "solid", "dotted")
			links_df$part_col <- ifelse(links_df$link %in% edges$link, "grey85", "grey40")
		
			set.seed(20)
			fig <- GGally::ggnet2(gnet, mode = "fruchtermanreingold",
						alpha=0.8, size=3)
		
			coord <- fig$data[,c("label", "x", "y")]
		
			hsv <- dplyr::full_join(
						hsv_test %>%
							dplyr::mutate(id=as.character(id)) %>%
							dplyr::filter(year==year) %>%
							dplyr::select(-year),
						coord,
						by=c("id"="label"))

		col_code <- function(x, legend=FALSE){
			cols <- viridis::viridis(3, option=palette)
			data_prep <- function(x){
				col <- ifelse(unique(hsv_test$cat[hsv_test$id==x])=="F", cols[2],
							ifelse(unique(hsv_test$cat[hsv_test$id==x])=="M", cols[1], cols[3]))
				temp <- hsv_test[hsv_test$id==x,] %>%
							dplyr::mutate(
								sample=factor(sample,
									levels = rev(c(2, 4, 3, 1))),
								val=0.25,
								col=ifelse(genome==1, col, "white")) %>%
							dplyr::select(-dna) %>%
							dplyr::select(-cat) %>%
							dplyr::select(-year)
				if(unique(hsv_test$cat[hsv_test$id==x])=="M"){
					fill <- data.frame(
								sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
								genome=0,
								id=x,
								val=0.25,
								col="grey") %>%
							dplyr::filter(sample != "4")
					miss <- !(fill$sample %in% temp$sample)
					temp <- rbind(temp, fill[miss,])
				}else{
					if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=x,
									val=0.25,
									col="grey") %>%
								dplyr::filter(sample == "1")
						miss <- !(fill$sample %in% temp$sample)
						temp <- rbind(temp, fill[miss,])
					}else{
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=x,
									val=0.25,
									col="grey")
						miss <- !(fill$sample %in% temp$sample)
						temp <- rbind(temp, fill[miss,])
					}
				}
		
				cols <- temp$col[order(as.integer(temp$sample))]
				return(list(temp, cols))
			}
				temp_d <- data_prep(x)
				temp <- temp_d[[1]]
				cols <- temp_d[[2]]
	
				pie <- ggplot2::ggplot(temp) +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.75) +
							ggplot2::coord_polar("y", start=0) +
							ggplot2::scale_fill_manual(values=cols) +
							ggplot2::theme_void() +
							ggplot2::theme(legend.position = "none")
				if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col=NA)
				}else{
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.5)
				}
				return(pie)
		}
			links_net <- setNames(data.frame(links_df), c("from", "to", "link", "part", "part_col")) %>%
							dplyr::left_join(., coord, by=c(c("to"="label"))) %>%
							dplyr::rename(xmax=x) %>%
							dplyr::rename(ymax=y) %>%
							dplyr::left_join(., coord, by=c(c("from"="label")))
		
			subplots <- lapply(coord$label[coord$label %in% hsv$id[!is.na(hsv$sample)]], col_code, legend=FALSE)
			
			frame <- ggplot2::ggplot() +
						ggplot2::geom_point(
							data=coord,
							ggplot2::aes(x, y),
							color=NA) +
						ggplot2::geom_segment(data=links_net,
							ggplot2::aes(x=x*1.25, xend=xmax*1.25, y=y*1.25, yend=ymax*1.25),
							color="grey80",
							linetype="solid", size=0.8) +
						ggplot2::theme_void()
		
			for(i in 1:length(subplots)){
				frame <- frame +
							ggplot2::annotation_custom(
								ggplot2::ggplotGrob(subplots[[i]]), 
									x = (coord$x[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]-0.016)*1.25,
									y = (coord$y[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]-0.016)*1.25, 
									xmax = (coord$x[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]+0.016)*1.25,
									ymax = (coord$y[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]+0.016)*1.25)
			}
			frame <- frame +
						ggplot2::geom_point(data=coord[!(coord$label %in% hsv$id[!is.na(hsv$sample)]),],
							ggplot2::aes(x=x*1.25, y=y*1.25), shape=21, color="black", fill="grey80", size=3)

			return(list(frame, degree_dist))
	}

	sub_fig_2015 <- graph_net(year=2015, palette_v=palette, show_legend=FALSE, legend_position=legend_pos)
	sub_fig_2016 <- graph_net(year=2016, palette_v=palette, show_legend=FALSE, legend_position=legend_pos)

	ggplot2::ggsave(
		filename=file.path(path.workspace, "figure_5_degree_2015.eps"),
		sub_fig_2015[[2]],
		width=4,
		height=4
	)

	ggplot2::ggsave(
		filename=file.path(path.workspace, "figure_5_degree_2016.eps"),
		sub_fig_2016[[2]],
		width=4,
		height=4
	)

	ggplot2::ggsave(
		filename=file.path(path.workspace, "figure_5_net_2015.eps"),
		sub_fig_2015[[1]],
		width=8,
		height=8
	)

	ggplot2::ggsave(
		filename=file.path(path.workspace, "figure_5_net_2016.eps"),
		sub_fig_2016[[1]],
		width=8,
		height=8
	)

	note("Figure saved...\n")
}

# Necessary function for data management --------------------------------------------------------------------
net_d_manage <- function(data=net2016, verbose=TRUE){
	var <- c("id", paste0("id_l", 1:10))
	ind <- cbind(data[[1]]$id, data[[1]][,-which(colnames(data[[1]]) %in% var)])
	colnames(ind)[1] <- "id"
	ind$id <- as.integer(ind$id)

	child <- data[[2]] %>%
				dplyr::mutate(id=as.integer(id))

	net1 <- data[[1]][,var] %>%
				tidyr::gather(., link, id_link, id_l1:id_l10) %>%
				dplyr::filter(!is.na(id_link)) %>%
				dplyr::select(id, id_link) %>%
				dplyr::mutate(id=as.integer(id)) %>%
				dplyr::filter(id!=id_link)
	net1$link <- apply(net1[, c("id", "id_link")], 1, function(x){
		paste(sort(x), collapse="-")
	})

	net2 <- child[,var] %>%
				tidyr::gather(., link, id_link, id_l1:id_l10) %>%
				dplyr::filter(!is.na(id_link)) %>%
				dplyr::select(id, id_link) %>%
				dplyr::mutate(id_link=as.integer(id_link)) %>%
				dplyr::filter(id!=id_link)
	net2$link <- apply(net2[, c("id", "id_link")], 1, function(x){
		paste(sort(x), collapse="-")
	})

	net <- rbind(net1, net2) %>%
				dplyr::mutate(dup=duplicated(link)) %>%
				dplyr::filter(dup==FALSE) %>%
				dplyr::select(id, id_link)

	if(verbose){
		note(paste0("\nNumber of children with links to id not present in the main dataset: ", length(unique(net2$id[!(net2$id_link %in% ind$id)])), "\n"))
	
		if(length(unique(net2$id[!(net2$id_link %in% ind$id)]))>0){
			note(paste0("ID of the children: ", paste(unique(net2$id[!(net2$id_link %in% ind$id)]), collapse=", "), "\n"))
		}
	}
	return(list(links=net, nodes=ind))
}
