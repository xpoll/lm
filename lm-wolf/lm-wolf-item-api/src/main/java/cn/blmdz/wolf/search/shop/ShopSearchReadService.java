package cn.blmdz.wolf.search.shop;

import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.search.dto.SearchedShopWithAggs;

public interface ShopSearchReadService {
	Response<SearchedShopWithAggs> searchWithAggs(Integer paramInteger1, Integer paramInteger2, String paramString,
			Map<String, String> paramMap);
}