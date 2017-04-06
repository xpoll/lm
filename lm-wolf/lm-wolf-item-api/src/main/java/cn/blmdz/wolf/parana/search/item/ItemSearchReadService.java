package cn.blmdz.wolf.parana.search.item;

import java.util.Map;

import cn.blmdz.home.common.model.Response;

public interface ItemSearchReadService {
   Response searchWithAggs(Integer var1, Integer var2, String var3, Map var4);
}
