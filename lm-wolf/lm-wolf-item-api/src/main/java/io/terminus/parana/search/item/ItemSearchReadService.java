package io.terminus.parana.search.item;

import io.terminus.common.model.Response;
import java.util.Map;

public interface ItemSearchReadService {
   Response searchWithAggs(Integer var1, Integer var2, String var3, Map var4);
}
