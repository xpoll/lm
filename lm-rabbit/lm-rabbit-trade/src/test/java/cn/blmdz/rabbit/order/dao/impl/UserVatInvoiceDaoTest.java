package cn.blmdz.rabbit.order.dao.impl;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.rabbit.order.dao.BaseDaoTest;
import cn.blmdz.rabbit.order.dao.UserVatInvoiceDao;
import cn.blmdz.rabbit.order.model.UserVatInvoice;

/**
 * Desc:
 * Mail: hehaiyang@terminus.io
 * Date: 16/3/17
 */
public class UserVatInvoiceDaoTest extends BaseDaoTest {

    @Autowired
    private UserVatInvoiceDao userVatInvoiceDao;

    private UserVatInvoice userVatInvoice;

    private UserVatInvoice createOne(Long id){
        userVatInvoice = new UserVatInvoice();
        userVatInvoice.setUserId(id);
        userVatInvoice.setCompanyName("abc");
        userVatInvoice.setBankAccount("123");
        userVatInvoice.setRegisterAddress("123");
        userVatInvoice.setRegisterBank("123");
        userVatInvoice.setRegisterPhone("123");
        userVatInvoice.setTaxCertificate("123");
        userVatInvoice.setTaxpayerCertificate("123");
        userVatInvoice.setTaxRegisterNo("123");
        return userVatInvoice;
    }

    @Before
    public void before(){
        for (int i = 0; i < 10; i++) {
            UserVatInvoice model = createOne(Long.valueOf(i));
            userVatInvoiceDao.create(model);
        }
    }

    @Test
    public void testAll(){
        testFindBys();
        testFindByUserId();
        testPaging();
        testUpdate();
    }

    public void testFindBys() {
        UserVatInvoice model =  userVatInvoiceDao.findById(1L);
        assertThat(model.getCompanyName(), is("abc"));

        List<UserVatInvoice> moderList = userVatInvoiceDao.findByIds(Arrays.asList(new Long[]{1L, 2L, 3L}));
        assertNotNull(moderList);
    }

    public void testFindByUserId() {
        UserVatInvoice model =  userVatInvoiceDao.findByUserId(1L);
        assertThat(model.getCompanyName(), is("abc"));
    }

    public void testPaging(){
        Paging<UserVatInvoice> modelList = userVatInvoiceDao.paging(1, 5);
        assertThat(modelList.isEmpty(), is(false));
    }

    public void testUpdate() {
        UserVatInvoice model = new UserVatInvoice();
        model.setId(3L);
        model.setCompanyName("def");
        Boolean result = userVatInvoiceDao.update(model);
        assertThat(result, is(true));
    }

}
