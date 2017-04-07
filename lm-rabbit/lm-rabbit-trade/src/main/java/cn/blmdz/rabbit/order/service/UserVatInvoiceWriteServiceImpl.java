package cn.blmdz.rabbit.order.service;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkState;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.dao.UserVatInvoiceDao;
import cn.blmdz.rabbit.order.model.UserVatInvoice;
import cn.blmdz.rabbit.order.service.UserVatInvoiceWriteService;
import lombok.extern.slf4j.Slf4j;

/**
 * Desc:
 * Mail: hehaiyang@terminus.io
 * Date: 16/3/17
 */
@Slf4j
@Service
public class UserVatInvoiceWriteServiceImpl implements UserVatInvoiceWriteService {

    @Autowired
    private UserVatInvoiceDao userVatInvoiceDao;

    /**
     * 创建增值税发票定义
     *
     * @param userVatInvoice 待创建或更新的增值税发票定义
     * @param user            创建增值税发票的用户信息
     * @return 创建或更新成功的增值税发票id
     */
    @Override
    public Response<Long> create(UserVatInvoice userVatInvoice, BaseUser user) {
        Response<Long> result = new Response<Long>();
        try {
            checkPersistArguments(userVatInvoice);
            UserVatInvoice existed = userVatInvoiceDao.findByUserId(user.getId());
            checkState(existed == null, "user.vat.invoice.already.exists");

            userVatInvoice.setUserId(user.getId());
            userVatInvoiceDao.create(userVatInvoice);
            result.setResult(userVatInvoice.getId());

        } catch (IllegalArgumentException e) {
            log.error("fail to create userVatInvoice with userVatInvoice:{}, error:{}", userVatInvoice, e.getMessage());
            result.setError(e.getMessage());
        } catch (IllegalStateException e) {
            log.error("fail to create userVatInvoice with userVatInvoice:{}, error:{}", userVatInvoice, e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("fail to create userVatInvoice with userVatInvoice:{}, cause:{}", userVatInvoice, Throwables.getStackTraceAsString(e));
            result.setError("user.vat.invoice.query.fail");
        }
        return result;
    }

    /**
     * 更新增值税发票定义
     *
     * @param userVatInvoice  待更新的增值税发票定义
     * @param user            更新增值税发票的用户信息
     * @return  是否执行成功
     */
    @Override
    public Response<Boolean> update(UserVatInvoice userVatInvoice, BaseUser user) {
        Response<Boolean> result = new Response<Boolean>();
        try {
            checkPersistArguments(userVatInvoice);
            checkArgument(userVatInvoice.getId() != null, "user.vat.invoice.id.can.not.be.empty");
            UserVatInvoice existed = userVatInvoiceDao.findById(userVatInvoice.getId());
            checkState(existed != null, "user.vat.invoice.not.found");
            checkState(Objects.equal(existed.getUserId(), user.getId()), "user.vat.invoice.not.belong.to.user");

            userVatInvoice.setUserId(user.getId());
            Boolean success = userVatInvoiceDao.update(userVatInvoice);
            result.setResult(success);

        } catch (IllegalArgumentException e) {
            log.error("fail to update userVatInvoice with userVatInvoice:{}, error:{}", userVatInvoice, e.getMessage());
            result.setError(e.getMessage());
        } catch (IllegalStateException e) {
            log.error("fail to update userVatInvoice with userVatInvoice:{}, error:{}", userVatInvoice, e.getMessage());
            result.setError(e.getMessage());
        } catch (Exception e) {
            log.error("fail to update userVatInvoice with userVatInvoice:{}, cause:{}", userVatInvoice, Throwables.getStackTraceAsString(e));
            result.setError("user.vat.invoice.update.fail");
        }

        return result;
    }

    private void checkPersistArguments(UserVatInvoice userVatInvoice) {
        checkArgument(!Strings.isNullOrEmpty(userVatInvoice.getCompanyName()), "company.name.can.not.be.empty");
        checkArgument(!Strings.isNullOrEmpty(userVatInvoice.getTaxRegisterNo()), "tax.register.no.can.not.be.empty");
        checkArgument(!Strings.isNullOrEmpty(userVatInvoice.getRegisterAddress()), "register.address.can.not.be.empty");
        checkArgument(!Strings.isNullOrEmpty(userVatInvoice.getRegisterPhone()), "register.phone.can.not.be.empty");
        checkArgument(!Strings.isNullOrEmpty(userVatInvoice.getRegisterBank()), "register.bank.can.not.be.empty");
        checkArgument(!Strings.isNullOrEmpty(userVatInvoice.getBankAccount()), "bank.account.can.not.be.empty");
        checkArgument(!Strings.isNullOrEmpty(userVatInvoice.getTaxCertificate()), "tax.certificate.can.not.be.empty");
        checkArgument(!Strings.isNullOrEmpty(userVatInvoice.getTaxpayerCertificate()), "tax.payer.certificate.can.not.be.empty");
    }
}
