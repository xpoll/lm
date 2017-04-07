package cn.blmdz.rabbit.web.core;

import org.junit.Test;

import cn.blmdz.aide.email.EmailException;

/**
 * Created by zhanghecheng on 16/3/11. <br>
 * Mail: zhanghecheng@terminus.io
 */
public class ExceptionMessageTest {

    private void emailEx(){
        throw new EmailException("eroor");
    }
    @Test
    public void test(){
        try{
            emailEx();
        }catch (EmailException e){
            System.out.println(e.getMessage());
        }
    }
}
