package io.terminus.galaxy.order.service;

import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.model.UserVatInvoice;

/**
 * Desc: 增值税开票信息写服务
 * Mail: hehaiyang@terminus.io
 * Date: 16/3/17
 */
public interface UserVatInvoiceWriteService {

    /**
     * 更新增值税发票定义
     *
     * @param userVatInvoice  待创建的增值税发票定义
     * @param user            创建增值税发票的用户信息
     * @return  创建成功的增值税发票id
     */
    Response<Long> create(UserVatInvoice userVatInvoice, BaseUser user);

    /**
     * 更新增值税发票定义
     *
     * @param userVatInvoice  待更新的增值税发票定义
     * @param user            更新增值税发票的用户信息
     * @return  是否执行成功
     */
    Response<Boolean> update(UserVatInvoice userVatInvoice, BaseUser user);
}
