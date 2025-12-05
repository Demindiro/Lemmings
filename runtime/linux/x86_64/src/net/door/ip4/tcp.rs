use crate::{linux, syscall};
use core::mem;
use lemmings_idl_net_ip4_tcp::*;

door! {
    [lemmings_idl_net_ip4_tcp Tcp "IPv4 TCP network interface"]
    listen
    connect
    accept
    recv
    recv_full
    send
    shutdown
    close_listener
    close_connection
}

#[repr(C)]
struct SockAddrIn {
    family: u16,
    port: u16,
    addr: u32,
    // who the fuck knows?
    _pad: u64,
}

fn connect(x: Connect) -> ConnectResult {
    let Connect {
        addr,
        port,
        timeout,
    } = x;
    let fd = unsafe { syscall::socket(syscall::AF_INET.into(), syscall::SOCK_STREAM, 0) };
    if fd < 0 {
        todo!("convert error {fd}");
    }
    let addr = SockAddrIn {
        family: syscall::AF_INET,
        port: u16::from(port).to_be(),
        addr: u32::from(addr).to_be(),
        _pad: 0,
    };
    let res = unsafe { syscall::connect(fd, &addr as *const _ as _, mem::size_of_val(&addr)) };
    if res >= 0 {
        return ConnectOk {
            socket: (fd as u32).into(),
        }
        .into();
    }
    unsafe { syscall::close(fd) };
    //let socket = u32::MAX.into();
    let socket = 0xcafebabe.into();
    let error = match res {
        linux::ECONNREFUSED => ConnectionRefused.into(),
        _ => todo!("convert error {res}"),
    };
    ConnectFail { socket, error }.into()
}

fn listen(x: Listen) -> ListenResult {
    todo!();
}

fn accept(socket: Listener) -> NewConnection {
    todo!();
}

fn recv(x: Recv) -> RecvResult {
    todo!();
}

fn recv_full(x: Recv) -> RecvResult {
    todo!();
}

fn send(x: Send) -> SendResult {
    let Send {
        socket,
        base,
        length,
    } = x;
    let fd = u32::from(socket) as i32;
    let mut base = base.0.cast().as_ptr();
    let mut len = usize::from(length);
    let mut total = 0;
    while len > 0 {
        let num = unsafe { syscall::write(fd, base, len) };
        if num == 0 {
            break;
        } else if num < 0 {
            todo!("handle err {num}");
        }
        base = unsafe { base.byte_add(num as usize) };
        len -= num as usize;
    }
    SendOk {
        length: total.into(),
    }
    .into()
}

fn shutdown(x: Shutdown) -> ShutdownResult {
    todo!();
}

fn close_listener(socket: Listener) {
    todo!();
}

fn close_connection(socket: Connection) {
    todo!();
}
