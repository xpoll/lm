package cn.blmdz.home.common.model;

import com.google.common.base.MoreObjects;

import java.io.Serializable;

/**
 * SOA RPC 调用封装对象，判断传输数据正确与否以及错误信息展示
 * email:dong_peiji@huateng.com
 * Created by 董培基 on 2016/3/2.
 */
public class Response<T> implements Serializable {
    private static final long serialVersionUID = -4612301430071730437L;
    private boolean success;
    private T result;
    private String error;

    public Boolean isSuccess() {
        return this.success;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }

    public T getResult() {
        return this.result;
    }

    public void setResult(T result) {
        this.success = true;
        this.result = result;
    }

    public String getError() {
        return error;
    }

    public void setError(String error) {
        this.success = false;
        this.error = error;
    }

    public static <T> Response<T> ok(T data) {
        Response<T> resp = new Response<>();
        resp.setResult(data);
        return resp;
    }

    public static <T> Response<T> fail(String error) {
        Response<T> resp = new Response<>();
        resp.setError(error);
        return resp;
    }

    public static <T> Response<T> ok() {
        return ok(null);
    }


    public String toString() {
        return MoreObjects.toStringHelper(this).add("result", result).add("error", error).add("success", success).omitNullValues().toString();
    }


}
