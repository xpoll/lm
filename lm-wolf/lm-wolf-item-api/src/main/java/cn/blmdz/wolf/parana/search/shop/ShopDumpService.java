package cn.blmdz.wolf.parana.search.shop;

import cn.blmdz.home.common.model.Response;

public interface ShopDumpService {
	Response<Boolean> fullDump();

	Response<Boolean> deltaDump(Integer paramInteger);
}