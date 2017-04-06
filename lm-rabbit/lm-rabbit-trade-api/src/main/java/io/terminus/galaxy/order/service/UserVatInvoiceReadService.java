package io.terminus.galaxy.order.service;

import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.model.UserVatInvoice;
import io.terminus.pampas.client.Export;

/**
 * Desc: 增值税开票信息读服务
 * Mail: hehaiyang@terminus.io
 * Date: 16/3/17
 */
public interface UserVatInvoiceReadService {

    /**
     * 获取指定用户的增值税发票定义
     *
     * @param userId 用户id
     * @return 增值税发票定义信
     */
    Response<UserVatInvoice> getByUserId(Long userId);

    /**
     * 获取当前用户的增值税发票定义
     *
     * @param user  用户
     * @return 增值税发票定义信
     */
    @Export(paramNames = {"user"})
    Response<UserVatInvoice> getByUser(BaseUser user);

}
