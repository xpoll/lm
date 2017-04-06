package io.terminus.galaxy.user.impl.dao;

import io.terminus.common.model.PageInfo;
import io.terminus.common.model.Paging;
import io.terminus.galaxy.user.model.Notice;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

/**
 * Created Date : 16/7/29
 * Author : wujianwei
 */
public class NoticeDaoTest extends BaseDaoTest {

    @Autowired
    private NoticeDao noticeDao;

    private Notice notice;

    public NoticeDaoTest() {
    }

    @Before
    public void init(){
        notice = new Notice();
        notice.setTitle("温州江南皮革城倒闭了");
        notice.setContext("老板带着小姨子跑了...还我血汗钱..");
        notice.setLevel(2);
        notice.setStatus(1);
        notice.setType(2);
        notice.setExtra("nothing");
        notice.setPicUrl("abc/def/here");
        notice.setShopId(9999L);
    }
    @Test
    public void testFindAll(){
        noticeDao.create(notice);
        assertThat(noticeDao.findAll().size(),is(2));
    }

    @Test
    public void testUpdateStatus() throws Exception {
        noticeDao.create(notice);
        noticeDao.findAll().get(0);
        noticeDao.updateStatus(noticeDao.findAll().get(0).getId(),2);
        assertThat(noticeDao.findAll().get(0).getStatus(),is(2));
    }

    @Test
    public void testDelete(){
        noticeDao.create(notice);
        assertThat(noticeDao.delete(noticeDao.findAll().get(0).getId()),is(true));
    }

    @Test
    public void testCreateNotice(){
        assertThat(noticeDao.create(notice),is(true));
    }

    @Test
    public void testPaging(){
        Notice notice = new Notice();
        notice.setShopId(9999L);
        PageInfo page = new PageInfo(null,null);
        noticeDao.create(notice);
        notice.setId(null);
        notice.setStatus(1);
        noticeDao.create(notice);
        Paging<Notice> paging = noticeDao.paging(page.getOffset(), page.getLimit(), notice);
        System.out.println(paging.getData());
    }
}
