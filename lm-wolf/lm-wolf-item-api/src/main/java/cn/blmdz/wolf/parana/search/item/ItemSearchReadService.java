package cn.blmdz.wolf.parana.search.item;

import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.search.dto.SearchedItemInShopWithAggs;
import cn.blmdz.wolf.parana.search.dto.SearchedItemWithAggs;

public interface ItemSearchReadService {
	public abstract Response<SearchedItemWithAggs> searchWithAggs(Integer paramInteger1, Integer paramInteger2,
			String paramString, Map<String, String> paramMap);

	public abstract Response<SearchedItemInShopWithAggs> searchInShopWithAggs(Integer paramInteger1,
			Integer paramInteger2, String paramString, Map<String, String> paramMap);
}
