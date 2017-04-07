package cn.blmdz.rabbit.user.impl.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.user.impl.dao.NoticeDao;
import cn.blmdz.rabbit.user.model.Notice;
import cn.blmdz.rabbit.user.service.NoticeWriteService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created Date : 16/7/29
 * Author : wujianwei
 */
@Service
@Slf4j
public class NoticeWriteServiceImpl implements NoticeWriteService {
    private final NoticeDao noticeDao;
    @Autowired
    public NoticeWriteServiceImpl(NoticeDao noticeDao) {
        this.noticeDao = noticeDao;
    }

    @Override
    public Response<Boolean> createNotice(Notice notice) {
        try {
            notice.setStatus(0);
            return Response.ok(noticeDao.create(notice));
        }catch(Exception e) {
            log.error("create notice failed, cause:{}", Throwables.getStackTraceAsString(e));
            return Response.fail("notice.create.failed");
        }

    }

    @Override
    public Response<Boolean> deleteNotice(Long noticeId) {
        try {
            return Response.ok(noticeDao.delete(noticeId));
        }catch(Exception e) {
            log.error("delete notice by noticeId={} failed, cause:{}", noticeId,Throwables.getStackTraceAsString(e));
            return Response.fail("notice.delete.failed");
        }
    }

    @Override
    public Response<Boolean> updateStatus(Long noticeId, Integer status) {
        try {
            return Response.ok(noticeDao.updateStatus(noticeId,status));
        }catch(Exception e) {
            log.error("update notice status failed,noticeId={}, cause:{}",noticeId, Throwables.getStackTraceAsString(e));
            return Response.fail("noticeStatus.update.failed");
        }
    }

    @Override
    public Response<Boolean> updateNoticeById(Notice notice) {
        try {
            return Response.ok(noticeDao.updateNotice(notice));
        }catch(Exception e) {
            log.error("update notice failed,noticeId={}, cause:{}",notice.getId(), Throwables.getStackTraceAsString(e));
            return Response.fail("notice.update.failed");
        }
    }

    @Override
    public Response<Boolean> disable(Long noticeId) {
        try {
            return Response.ok(noticeDao.updateStatus(noticeId, 2));
        } catch (Exception e) {
            log.error("disable notice failed, noticeId={}, error={}",
                    noticeId, Throwables.getStackTraceAsString(e));
            return Response.fail("notice.disable.failed");
        }
    }
}
