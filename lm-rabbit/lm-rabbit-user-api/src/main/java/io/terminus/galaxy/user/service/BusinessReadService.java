package io.terminus.galaxy.user.service;

import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.model.Business;
import io.terminus.pampas.client.Export;

import java.util.List;

/**
 * Created by liushaofei on 16/7/29.
 */
public interface BusinessReadService {

    /**
     * 只要改条件存在就进行查询,并且进行的是"=", 查询条件不支持范围参数eg:时间等等
     * @param
     * @return
     */
    Response<Paging<Business>> select(Business business,Integer pageIndex, Integer pageSize);

    @Export(
            paramNames = {"businessName", "type", "pageNo", "pageSize"}
    )
    Response<Paging<Business>> selectByBusinessNameAndType(String businessName, Integer type, Integer pageNo, Integer pageSize);

    List<Business> findByIds(List<Long> ids);

    Business findById(Long id);

}
