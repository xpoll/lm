package cn.blmdz.wolf.search.dto;

import java.io.Serializable;
import java.util.List;

import cn.blmdz.home.common.model.Paging;
import lombok.Data;

@Data
public class SearchedItemInShopWithAggs implements Serializable {
	private static final long serialVersionUID = -9071820512294834526L;
	private Paging<SearchedItem> entities;
	private List<GroupedAggNav> attributes;
	private List<AggNav> shopCategories;
	private List<IdAndName> breadCrumbs;
	private List<Chosen> chosen;

}